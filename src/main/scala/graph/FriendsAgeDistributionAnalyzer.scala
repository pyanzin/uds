package uds.graph

import uds._
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge._
import scalax.collection.edge.Implicits._
import uds.Helpers._

case class FriendsAgeDistr(value: Map[Int, Int]) extends SimpleProp[Map[Int, Int]] {
  def identifier = s"vkfad"
}

class FriendsAgeDistributionAnalyzer(user: User) extends Block[VkGraph, VkGraph] {
  def apply(g: VkGraph): VkGraph = {
    g ++ Graph.from(List(), List(
      LUnDiEdge(user, FriendsAgeDistr(getProp(g)))("prop").asInstanceOf[LUnDiEdge[VkNode]]))
  }

  def getProp(g: VkGraph) = {
    val userNode = g.get(user)

    userNode.outgoing
      .filter(_.label == "friend").flatten
      .toList
      .collect(_.value match { case u: User => u.birthYear })
      .flatten
      .distribution(identity)
  }
}