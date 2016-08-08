package uds.actor

import akka.actor._
import redis._

class RedisNotificationActor extends Actor {
	val redis = new RedisClient()

	def receive = {
		case Progress(name, ratio) => redis.publish("response", 
			s"""{"typeName": "progress", "name": "$name", $ratio}""")
	}
}