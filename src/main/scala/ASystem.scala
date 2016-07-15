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

	vk ! FriendsRequest(targetId)

	def receive = {
//		case ar: AnalysisRequest => {
//			val vk = new Vk("")
//	    	val loader = new InitialLoader(vk)
//	    	sender ! loader(ar.targetId)
//		}

		case users: Seq[User] => {

		}

		case (id: Long, friendIds: Seq[Long]) => {

		}
	}

	def name = s"initial-$targetId-$clientId"
}

class VkActor extends Actor {
	def receive = {
		case request: VkRequest[_] => {
			implicit val materializer = akka.stream.ActorMaterializer()

			val future: Future[HttpResponse] = Http(context.system).singleRequest(HttpRequest(uri = request.uri))

			future.onComplete {
				case Success(x) => sender ! (request.convert(x.toString))				
			}
		}
	}
}

abstract class VkRequest[T] {
	def uri: String 
	def convert(s: String): T
}

case class UsersRequest(ids: Long*) extends VkRequest[Seq[User]] {
	implicit val formats = DefaultFormats
    import net.liftweb.json.JsonParser._

	val uri = "https://api.vk.com/method/users.get?user_ids=" + ids.mkString(",")

	def convert(s: String): Seq[User] = {
		val usersJson = parse(s) \\ "response" 
        usersJson.extract[List[User]]
	}
}

case class FriendsRequest(id: Long) extends VkRequest[(Long, Seq[Long])] {
	implicit val formats = DefaultFormats
    import net.liftweb.json.JsonParser._

	val uri = "https://api.vk.com/method/friends.get?user_id=" + id

	def convert(s: String): (Long, Seq[Long]) = {
		val usersJson = parse(s) \\ "response" 
        id -> usersJson.extract[List[Long]]
	}
}

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
