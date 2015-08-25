package uds.graph

import uds._
import uds.graph._
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge._
import scalax.collection.edge.Implicits._
import net.liftweb.json._

case class Group(
  id: Long,
  name: String,
  displayName: String,
  isClosed: Boolean,
  isPublicPage: Boolean
) extends VkNode

class GroupsLoader(vk: Vk) extends Block[Graph[VkNode, LUnDiEdge], Graph[VkNode, LUnDiEdge]] {
  def toGroup(x: JValue): Option[Group] = x match {
    case JObject => Some(Group(
      x \ "gid" match {case JInt(x) => x.toLong },
      x \ "name" match { case JString(x) => x },
      x \ "screen_name" match { case JString(x) => x },
      x \ "is_closed" match {
        case JInt(x) if x==1 => true
        case _ => false
      },
      x \ "type" match {
        case JString("group") => false
        case _ => true
      }
    ))
    case _ => None
  }

  def getGroups(userId: Long) = {
    val json = vk.vkMethod("groups.get", "user_id" -> userId.toString, "extended" -> "1")
    val JArray(groupsJson) = parse(json) \\ "response"
    groupsJson flatMap toGroup
  }
  def apply(g: Graph[VkNode, LUnDiEdge]): Graph[VkNode, LUnDiEdge] = {
    val users = g.nodes.map(_.value).collect{ case u: User => u }
    val groupsRels = users flatMap { u =>
      val groups = getGroups(u.id)
      groups.map { g =>
        LUnDiEdge(u, g)("isMember").asInstanceOf[LUnDiEdge[VkNode]]
      }
    }

    Graph.from(users, groupsRels)
  }
}
