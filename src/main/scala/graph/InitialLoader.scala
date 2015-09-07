package uds.graph

import uds._
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge._
import scalax.collection.edge.Implicits._

class InitialLoader(vk: Vk) extends Block[Long, VkGraph] {
  def apply(id: Long): VkGraph = {
    val friendIds = vk.getFriends(id)
    val friends = vk.getUsers(id :: friendIds:_*)
    val index = friends.map(x => x.id -> x).toMap

    val rels = id :: friendIds flatMap { x =>
      try {
        vk.getFriends(x) intersect friendIds map {
          y => LUnDiEdge(index(y), index(x))("friend").asInstanceOf[LUnDiEdge[VkNode]] }
      } catch {
        case _: Exception => Nil
      }
    }
    Graph.from(friends, rels)
  }
}
