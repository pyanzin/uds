package uds.graph

import uds._
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge._
import scalax.collection.edge.Implicits._

trait VkNode

trait SimpleProp[T] extends VkNode {
  val value: T
}

case class FriendsCount[Int](value: Int) extends SimpleProp[Int]
