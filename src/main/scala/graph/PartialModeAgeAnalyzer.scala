package uds.graph

import uds._
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge._
import scalax.collection.edge.Implicits._
import uds.Helpers._

case class PartialAgeMode(value: Int) extends SimpleProp[Int] {
  def identifier = s"pamvk$value"
}

class PartialAgeModeAnalyzer(user: User) extends Block[VkGraph, VkGraph] {
  def apply(g: VkGraph): VkGraph = {
    g ++ Graph.from(List(), List(
      LUnDiEdge(user, PartialAgeMode(getProp(g)))("prop").asInstanceOf[LUnDiEdge[VkNode]]))
  }

  def getProp(g: VkGraph) = {
    val userNode = g.get(user)

    val dist = userNode.outgoing
      .filter(_.label == "friend").flatten
      .toList
      .collect(_.value match { case u: User => u.birthYear })
      .flatten
      .distribution(identity)

      uds.ConsoleHelpers.getMode(dist)
  }
}