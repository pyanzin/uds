import dispatch._, Defaults._

package uds {

  class TokenDealer(val login: String, val password: String, val appId: Int) {
    def req(u: String) = {
      val svc = url(u)
      val future = Http(svc OK as.String)
      future()
    }

    def httpRequest(url: String, params: (String, Any) *) = {
      val prms = params.map{ x => x._1 + "=" + x._2.toString }.mkString("&")
      req(s"$url?$prms")
    }

    val iphRegex = """(?s:.*)<input type=\"hidden\" name=\"ip_h\" value=\"(.*)\" />(?s:.*)""".r
    val toRegex = """(?s:.*)<input type="hidden" name="to" value="(.*)">(?s:.*)""".r
    
    def getRedirectUrl(uri: String, params: (String, Any) *) = {
      val prms = params map { x => x._1 + "=" + x._2.toString } mkString("&")
      val svc = url(s"$uri?$prms")
      val r = Http.configure(_ setFollowRedirects true)(svc > (x => x))
      r().getUri.toString
    }

    def getToken() = {
      val loginPage = httpRequest("https://oauth.vk.com/authorize",
        "client_id" -> appId,
        "display" -> "wap",
        "response_type" -> "token"
      )
      
      val iphRegex(ip_h) = loginPage
      val toRegex(to) = loginPage 

      val uri = getRedirectUrl("https://login.vk.com",
        "act" -> "login",
        "soft" -> "1",
        "q" -> "1",
        "ip_h" -> ip_h,
        "to" -> to,
        "from_host" -> "oauth.vk.com",
        "expire" -> "0",
        "email" -> login, 
        "pass" -> password
      )

      val params = uri.split("#")(1).split("&").map(_.split("=")).map(x => x(0) -> x(1)).toMap

      params("access_token")
    }
  }
}
