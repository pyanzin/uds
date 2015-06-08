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

    class Vk(val token: String) {
      implicit val formats = DefaultFormats
      import net.liftweb.json.JsonParser._

      def req(u: String) = {
        val svc = url(u)
        val future = Http(svc OK as.String)
        future()
      }

      def httpRequest(url: String, params: (String, String) *) = {
        val prms = params.map{ x => x._1 + "=" + x._2 }.mkString("&")
        req(s"$url?$prms")
      }

      def vkMethod(method: String, params: (String, String)*) =
          httpRequest(s"https://api.vk.com/method/$method",
            (("access_token" -> token) :: params.toList): _* )

      def getUsers(ids: Long *) = {
        Thread.sleep(400)
        val f = vkMethod("users.get", 
          "user_ids" -> ids.mkString(","),
          "fields" -> List("bdate", "city", "sex").mkString(",")
        )
        val JArray(usersJson) = parse(f) \\ "response" 
        usersJson map toUser
      }

      def getFriends(id: Long) = {
        Thread.sleep(400)
        val f = vkMethod("friends.get", "user_id" -> id.toString)
        (parse(f) \\ "response").extract[List[Long]]
      }
    }
}
