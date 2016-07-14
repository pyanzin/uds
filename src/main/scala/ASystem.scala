package uds.actor

import akka.actor._

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
 
import scala.concurrent.Future
import scala.util.{ Failure, Success }


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

class InitialAnalysisActor extends Actor {
	def receive = {
		case ar: AnalysisRequest => {
			val vk = new Vk("")
	    	val loader = new InitialLoader(vk)
	    	sender ! loader(ar.targetId)
		}
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
