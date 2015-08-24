import dispatch._, Defaults._
import net.liftweb.json._
import scala.concurrent.ExecutionContext.Implicits.global._

package object uds
{

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
          }
        )
    }

    class Vk(val token: String, delay: Int = 400) {
      implicit val formats = DefaultFormats
      import net.liftweb.json.JsonParser._

      var lastReqTime: Long = 0

      def req(u: String) = {
        val svc = url(u)
        val future = Http(svc OK as.String)
        future()
      }

      def delayedReq(u: String) = {
        val diff = System.currentTimeMillis - lastReqTime 
        if (diff < delay)
          Thread.sleep(diff)
        val result = req(u)
        lastReqTime = System.currentTimeMillis
        result
      }

      def httpRequest(url: String, params: (String, String) *) = {
        val prms = params.map{ x => x._1 + "=" + x._2 }.mkString("&")
        req(s"$url?$prms")
      }

      def vkMethod(method: String, params: (String, String)*) =
          httpRequest(s"https://api.vk.com/method/$method",
            (("access_token" -> token) :: params.toList): _* )

      def getUsers(ids: Long *) = {
        val f = vkMethod("users.get", 
          "user_ids" -> ids.mkString(","),
          "fields" -> List("bdate", "city", "sex").mkString(",")
        )
        val JArray(usersJson) = parse(f) \\ "response" 
        usersJson map toUser
      }

      def getFriends(id: Long) = {
        val f = vkMethod("friends.get", "user_id" -> id.toString)
        (parse(f) \\ "response").extract[List[Long]]
      }
    }
}
