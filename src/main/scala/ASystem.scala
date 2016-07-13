package uds.actor

import akka.actor._

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
 
import scala.concurrent.Future
import scala.util.{ Failure, Success }

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

def analyze = {
import uds._
import uds.graph._

class VkActor extends Actor {
  def receive = {
    case id: Long => {
    	val vk = new Vk("")
    	val loader = new InitialLoader(vk)
    	sender ! loader(id)
    }
  }
}

class AnalyzerActor extends Actor {
	def receive = {
    	case id: Long => context.actorSelection("../vk") ! id
    	case s: String => println(s)
    	case g: VkGraph => println(g)
  }
}

implicit val system = ActorSystem()

val vkActor = system.actorOf(Props(new VkActor), "vk")
val aActor = system.actorOf(Props(new AnalyzerActor), "analyzer")

}