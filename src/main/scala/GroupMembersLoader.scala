package uds.graph

import uds._
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge._
import scalax.collection.edge.Implicits._
import scala.util.Try

class GroupMembersLoader(vk: Vk) extends Block[Long, VkGraph] {

  def apply(id: Long): VkGraph = {
    val membersIds = Try(vk.getMembers(id)).getOrElse(List())
    val members = membersIds.grouped(250).flatMap(xs => Try(vk.getUsers(xs:_*)).getOrElse(List())).toList
    val index = members.map(x => x.id -> x).toMap

    val rels = membersIds flatMap { x =>
      Try(
        vk.getFriends(x) intersect membersIds map {
          y => LUnDiEdge(index(y), index(x))(Friend()).asInstanceOf[LUnDiEdge[VkNode]] }
      ).getOrElse(List())
    }

    Graph.from(members, rels)
  }
}
