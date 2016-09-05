package uds

import uds._
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge._
import scalax.collection.edge.Implicits._

package object graph {
  trait VkNode extends Product {
  	def identifier: String
  }

  case class Friend()
  
  type VkGraph = Graph[VkNode, LUnDiEdge]
  
  trait SimpleProp[T] extends VkNode {
    val value: T
  }
  
  case class FriendsCount[Int](value: Int) extends SimpleProp[Int] {
  	val identifier = s"vkfc$value"
  }
}