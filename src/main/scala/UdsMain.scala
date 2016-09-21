package uds

import uds._, uds.graph._
import scala.util.Try
import java.lang.Thread
import net.liftweb.json._
import net.liftweb.json.Serialization.{write, read}
import net.liftweb.json.Extraction.extract
import redis._
import redis.api.pubsub._
import akka.actor._
import uds.graph._
import uds.ConsoleHelpers.AnalysisRequest

object UdsMain extends App {
  override def main(args: Array[String]) = {
    implicit val formats = net.liftweb.json.Serialization.formats(NoTypeHints)

    val vk = new Vk("", 0)

    implicit val actorSystem = ActorSystem()

    val redisClient = RedisClient()

    def writeResponse(json: String) {
        redisClient.publish("response", json)
    }

    def analyze(request: String) {
        println(request)
        val ar = read[AnalysisRequest](request)

        ar.typeName match {
            case "initial" => {
                val g = new InitialLoader(vk)(ar.targetId)

                println(s"Initial analysis completed for ${ar.forId} with ${g.nodes.length} nodes and ${g.edges.length} edges")

                writeResponse(s"""{"typeName": "initial", "result": ${ConsoleHelpers.toD3Graph(g)}}""")
            }
            case "age" => {
                println(s"Age analysis completed for ${ar.forId}")

                writeResponse(s"""{"typeName": "age", "result": ${write(new DeepFriendsAgeModeAnalyzer(vk)(ar.targetId).map(x => x._1.toString -> x._2).toMap)}}""")
            }

            case "cities" => {
                println(s"Cities request completed for ${ar.forId}")

                val cityIds = vk.getUsers(vk.getFriends(ar.targetId):_*).flatMap(_.cityId).distinct
                writeResponse(s"""{"typeName": "cities", "result": ${write(vk.getCities(cityIds:_*))}}""")
            }
        }   
    }

    val sub = RedisPubSub(channels = List("request"), patterns = List("*"), onMessage = { case Message(p, m) => analyze(m.utf8String) })

  }
}
