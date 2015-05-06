package uds.graph

import uds._
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge._
import scalax.collection.edge.Implicits._

class InitialLoader(vk: Vk) extends Block[Long, Graph[User, LUnDiEdge]] {
  def apply(id: Long) = {
    val friendIds = vk.getFriends(id)
    val friends = vk.getUsers(friendIds:_*)
    val index = friends.map(x => x.id -> x).toMap

    val rels = id :: friendIds flatMap { x =>
      try {
        vk.getFriends(x) intersect friendIds map { y => index(y) -> index(x) }
      } catch {
        case _: Exception => Nil
      }
    }
    Graph.from(friends, rels map { x => (x._1 ~+ x._2)("friend") })
  }
}
