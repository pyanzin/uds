package uds

package object ConsoleHelpers {
  import uds.graph._
  def printAgeDistr(id: Long) = {
    import uds._, uds.graph._

    val vk = new Vk("", 0)

    val g = (new InitialLoader(vk))(id)

    val targetUser = g.nodes.collect(_.value match { case u: User if u.id == id => u}).head

    val analyzer = new FriendsAgeDistributionAnalyzer(targetUser)

    val d = analyzer.getProp(g)

    val absolute = 3
    val count = d.size.toDouble

    d.toList.sortBy(_._1).foreach(x => println(s"""${x._1}\t${"*"*(x._2 / count * absolute).toInt }"""))
  }

  def getMode[T](dist: Map[T, Int]): T = dist.maxBy(_._2)._1

  def getMean(dist: Map[Int, Int]) = dist.map(_._1).sum / dist.size

  def getMedian(dist: Map[Int, Int]) = {
  val half = dist.values.sum / 2
    dist.toList.sortBy(_._2).foldRight((0, 0))((x, z) =>
      if (z._2 > half)
        z
      else
        (x._1, z._2 + x._2)
    )._1
  }

  def fillGaps(dist: Map[Int, Int]) = {
    val min = dist.values.min
    val max = dist.values.max

    min to max map { x =>
      (x, dist.getOrElse(x, 0))
    }
  }

  def toD3Graph(graph: VkGraph): String = {
    import net.liftweb.json._
    import net.liftweb.json.Extraction.decompose

    implicit val formats = Serialization.formats(NoTypeHints)
    val nodes = graph.nodes.toList

    val nodesJ = JArray(nodes.map(x => decompose(x.value)).toList)

    val index = nodes.zipWithIndex.toMap
    val pairs = graph.edges.map(x => x.head -> x.last)

    def rel(a: graph.NodeT, b: graph.NodeT) = {
      a.value.asInstanceOf[User] hasSameName b.value.asInstanceOf[User]
    }
    val edgesJ = JArray(pairs.map(_ match { case (a, b) => 
      JObject(List(
        JField("source", JDouble(index(a))),
        JField("target", JDouble(index(b))),
        JField("value", JBool(rel(a, b)))
      ))
    }).toList)

    val ast = JObject(List(
        JField("nodes", nodesJ),
        JField("links", edgesJ)
      ))
    
    pretty(render(ast))
  }

  def toJson(m: Map[String, _]): String = {
    import net.liftweb.json._
    import net.liftweb.json.Extraction.decompose
    import net.liftweb.json.Serialization.{read, write}

    implicit val formats = Serialization.formats(NoTypeHints)

    write(m)
  }

  case class AnalysisRequest(forId: Long, targetId: Long, typeName: String)

  def startHandlerConsoleStyle() {
    import uds._, uds.graph._
    import scala.util.Try
    import java.lang.Thread
    import net.liftweb.json._
    import net.liftweb.json.Serialization.{write, read}
    import net.liftweb.json.Extraction.extract
    import redis._
    import redis.api.pubsub._
    import akka.actor._

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

                val cachedG = getFriendsGraphFromNeo(ar.targetId)

                if(cachedG.nodes.size > 0){
                  println(s"Cached graph for ${ar.forId} with ${cachedG.nodes.length} nodes and ${cachedG.edges.length} edges")

                  writeResponse(s"""{"typeName": "initial", "result": ${ConsoleHelpers.toD3Graph(cachedG)}}""")
                } else {                
                  val g = new InitialLoader(vk)(ar.targetId)

                  writeToDefaultNeo(g)

                  println(s"Analysis completed for ${ar.forId} with ${g.nodes.length} nodes and ${g.edges.length} edges")

                  writeResponse(s"""{"typeName": "initial", "result": ${ConsoleHelpers.toD3Graph(g)}}""")
                }
            }
            case "age" => {
                writeResponse(s"""{"typeName": "age", "result": ${write(new DeepFriendsAgeModeAnalyzer(vk)(ar.targetId).map(x => x._1.toString -> x._2).toMap)}}""")
            }

            case "cities" => {
                val cityIds = vk.getUsers(vk.getFriends(ar.targetId):_*).flatMap(_.cityId).distinct
                writeResponse(s"""{"typeName": "cities", "result": ${write(vk.getCities(cityIds:_*))}}""")
            }
        }   
    }

    val sub = RedisPubSub(channels = List("request"), patterns = List("*"), onMessage = { case Message(p, m) => analyze(m.utf8String) })

  }

  def writeToDefaultNeo(g: VkGraph) {
    import uds._, uds.graph._
    import org.neo4j.driver.v1._
    import uds.neo4j.nutils._

    val driver = GraphDatabase.driver("bolt://localhost", AuthTokens.basic("neo4j", "pass"))
    val session = driver.session()

    val saveRequest = toNeoRequest(g)

    session.run(saveRequest).consume()
  }

  def getFriendsGraphFromNeo(userId: Long): VkGraph = {
    import uds._, uds.graph._
    import org.neo4j.driver.v1._
    import uds.neo4j.nutils._

    val driver = GraphDatabase.driver("bolt://localhost", AuthTokens.basic("neo4j", "pass"))
    val session = driver.session()

    val aloneFriendsRequest = node[User](s"_.id = $userId").edge[Friend].node[User].build
    val socialFriendsRequest = node[User](s"_.id = $userId").edge[Friend].node[User].edge[Friend].node[User].edge[Friend].node[User](s"_.id = $userId").build

    aloneFriendsRequest.convert(session.run(aloneFriendsRequest.request)) ++ socialFriendsRequest.convert(session.run(socialFriendsRequest.request))
  }
}