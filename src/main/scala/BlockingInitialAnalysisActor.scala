package uds.actor

import uds._, uds.graph._
import scala.util.Try
import java.lang.Thread
import net.liftweb.json._
import net.liftweb.json.Serialization.{write, read}
import net.liftweb.json.Extraction.extract
import redis._
import redis.api.pubsub._
import akka.actor._

class BlockingInitialAnalysisActor(targetId: Long, clientId: Long) extends BaseAnalysisActor {

	val redis = new RedisClient()

	def receive = {
		case _ => {
		    val vk = new Vk("", 0)

			val g = new InitialLoader(vk)(targetId)

			redis.publish("response", s"""{"typeName": "initial", "result": ${ConsoleHelpers.toD3Graph(g)}}""")
		}
	}

	val name = s"$targetId-$clientId-initial"
	
}