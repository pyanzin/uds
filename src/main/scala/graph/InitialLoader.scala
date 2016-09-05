package uds.graph

import uds._
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge._
import scalax.collection.edge.Implicits._
import scala.util.Try

class InitialLoader(vk: Vk) extends Block[Long, VkGraph] {

  def apply(id: Long): VkGraph = {
    val friendIds = Try(vk.getFriends(id)).getOrElse(List())
    val friends = (id :: friendIds).grouped(250).flatMap(xs => Try(vk.getUsers(xs:_*)).getOrElse(List())).toList
    val index = friends.map(x => x.id -> x).toMap

    val rels = id :: friendIds flatMap { x =>
      Try(
        vk.getFriends(x) intersect friendIds map {
          y => LUnDiEdge(index(y), index(x))(Friend()).asInstanceOf[LUnDiEdge[VkNode]] }
      ).getOrElse(List())
    }
    Graph.from(friends, rels)
  }
}
