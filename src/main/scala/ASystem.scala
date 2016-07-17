package uds.actor

import akka.actor._

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
 
import scala.concurrent.Future
import scala.util.{ Failure, Success }

import dispatch._, Defaults._
import net.liftweb.json._
import scala.concurrent.ExecutionContext.Implicits.global._
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.http.scaladsl.model._
import akka.util._

import uds._
import uds.graph._

case class AnalysisRequest(name: String, clientId: Long, targetId: Long)

case class Progress(name: String, progress: Double)

abstract class BaseAnalysisActor extends Actor {
	def progress(x: Double) {
		notificator ! Progress(name, x)
	}

	def name: String

	def notificator = context.actorSelection("/user/notificator")
}

class InitialAnalysisActor(targetId: Long, clientId: Long) extends BaseAnalysisActor {

	val vk = context.actorOf(Props(new VkActor))

	var rels = Seq[(Long, Long)]()
	var users = Seq[User]()
	var directFriendIds = Seq[Long]()
	var toGetFriends = Set[Long]()

	vk ! FriendsRequest(targetId)

	def receive = {
		case xs: Seq[User] => {
			users ++= xs
		}

		case FriendsRelations(id: Long, friendIds: Seq[Long]) => {
			rels ++= friendIds.map(id -> _)
			if (id == targetId) {	
				directFriendIds = friendIds

				toGetFriends = Set(friendIds: _*)

				(List(id) ++ friendIds)
					.grouped(250)
					.map(x => UsersRequest(x: _*))
					.foreach(vk ! _)

				friendIds.foreach(x => vk ! FriendsRequest(x))
			} else {
				toGetFriends -= id

				if (toGetFriends.isEmpty)
					buildAndSendGraph()
			}
		}
	}

	def buildAndSendGraph() {
		println("graph built")
	}

	def name = s"initial-$targetId-$clientId"
}

class VkActor extends Actor {
	def receive = {
		case request: VkRequest[_] => {
			implicit val materializer = akka.stream.ActorMaterializer()

			val future: Future[HttpResponse] = Http(context.system).singleRequest(HttpRequest(uri = request.uri))

			future.onComplete {

				case Success(x) => {
					x.entity.dataBytes.runFold(ByteString(""))(_ ++ _).onComplete {
						case Success(body) => context.parent ! request.convert(body.utf8String)
					}
				}			
			}
		}
	}
}

abstract class VkRequest[T] {
	def uri: String 
	def convert(s: String): T

	def buildUri(base: String, params: (String, Any) *): String = {
        val prms = params.map{ x => x._1 + "=" + x._2.toString }.mkString("&")
        println(s"$base?$prms")
        s"$base?$prms"
    }

	def buildVkUri(method: String, params: (String, Any)*): String =
          buildUri(s"https://api.vk.com/method/$method",
            (("lang" -> "3") :: params.toList): _* )
}

case class UsersRequest(ids: Long*) extends VkRequest[Seq[User]] {
	implicit val formats = DefaultFormats
    import net.liftweb.json.JsonParser._

	val uri = buildVkUri("users.get",
		"user_ids" -> ids.mkString(","),
		"fields" -> List("bdate", "city", "sex", "photo_100").mkString(","))

	def convert(s: String): Seq[User] = {
		val JArray(usersJson) = parse(s) \\ "response" 
        usersJson map toUser
	}

	def toUser(x: JValue) = {
        User(x \ "uid" match { case JInt(x) => x.toLong },
          x \ "first_name" match { case JString(x) => x },
          x \ "last_name" match { case JString(x) => x },
          x \ "sex" match {
            case JInt(s) if s == 1 => Some(false)
            case JInt(s) if s == 2 => Some(true)
            case _ => None
          },
          x \ "bdate" match {
            case JString(x) => Some(x)
            case _ => None
          },
          x \ "city" match {
            case JInt(x) => Some(x.toInt)
            case _ => None
          },
          x \ "photo_100" match {
            case JString(x) => x
          }
        )
    }
}

case class FriendsRequest(id: Long) extends VkRequest[FriendsRelations] {
	implicit val formats = DefaultFormats
    import net.liftweb.json.JsonParser._

	val uri = buildVkUri("friends.get", "user_id" -> id)

	def convert(s: String): FriendsRelations = {
        FriendsRelations(id, (parse(s) \\ "response").extract[List[Long]])
	}
}

case class FriendsRelations(targetId: Long, friendsIds: Seq[Long])

//class ArbiterActor extends Actor {
//
//	def receive = {
//		case ar: AnalysisRequest => {
//			val a = context.actorOf(Props[InitialAnalysisActor])
//			a ! ar
//		}
//    	case g: VkGraph => println(g)		
//	}
//}


//package uds {
//
//	case class VkRequest
//
//	class VkActor extends Actor {
//		def receive = {
//			case r: VkRequest => {
//				implicit val system = ActorSystem()
//				implicit val materializer = akka.stream.ActorMaterializer()
//
//				val future = Http().singleRequest(HttpRequest(uri = "https://api.vk.com/method/users.get?user_ids=8385473"))
//				//future//.onSuccess(x => sender ! x.toString)
//			}
//		}
//	}
//
//}//
