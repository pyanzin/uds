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
    ConsoleHelpers.startHandlerConsoleStyle()
        
  }
}
