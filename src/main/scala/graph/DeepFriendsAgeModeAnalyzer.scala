package uds.graph

import uds._
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge._
import scalax.collection.edge.Implicits._
import uds.Helpers._
import scala.util.Try

class DeepFriendsAgeModeAnalyzer(vk: Vk) extends Block[Long, Map[Long, Int]] {
  def apply(id: Long): Map[Long, Int] = 
      vk.getUsers((id :: vk.getFriends(id): _*)).map(u => u.id -> getBirthYear(u)).toMap  

  def getBirthYear(u: User) = u.birthYear.getOrElse {
    val ids = Try(vk.getFriends(u.id)).getOrElse(Nil)
    val friends = ids.grouped(200).flatMap(x => Try(vk.getUsers(x: _*)).getOrElse(Nil)).toList
    val dist = friends.collect(x => x.birthYear match { case Some(y) => y }).distribution(identity)

    if (dist.isEmpty) 0 else ConsoleHelpers.getMode(dist)
  }
}