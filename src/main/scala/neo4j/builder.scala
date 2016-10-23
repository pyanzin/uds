package uds.neo4j

import org.neo4j.driver.v1._
import scala.collection.JavaConverters._
import uds._
import uds.graph._
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge._
import scalax.collection.edge.Implicits._

trait NeoRequestEntity {
    def rangeId: String
    def wherePart: Option[String]    
    def typeName: String
    def patternPart: String
}

case class Node[N <: VkNode](
    whereClause: Option[String] = None,
    previousEntities: List[NeoRequestEntity] = Nil,
    nodeCounter: Int = 0,
    edgeCounter: Int = 0)
    (implicit val mf: Manifest[N]) extends NeoRequestEntity {
    val rangeId = "n" + nodeCounter

    val wherePart: Option[String] = whereClause.map(_.replace("_", rangeId))

    val typeName = mf.runtimeClass.getSimpleName

    val patternPart: String = s"($rangeId:$typeName)"

    def edge[E](whereClause: String = null)(implicit mf: Manifest[E]) : Edge[E] = {
        Edge[E](if (whereClause == null) None else Some(whereClause),
            this :: previousEntities, nodeCounter + 1, edgeCounter)(mf)
    }

    def edge[E](implicit mf: Manifest[E]): Edge[E] = edge[E](null.asInstanceOf[String])(mf)


    def build: Builder = {
       val entities = (this :: previousEntities).reverse
       new Builder(entities)
    }

    override def toString() = build.request

    def extract(record: Record): N = {
        import net.liftweb.json._
        import net.liftweb.json.Extraction.decompose
        import net.liftweb.json.Serialization.{read, write}

        implicit val formats = Serialization.formats(NoTypeHints)

        val mutable = record.get(rangeId).asMap.asScala
        val map = Map(mutable.toSeq: _*)

        val json = decompose(map)
        json.extract[N](formats, mf)
    }
}

case class Edge[E](
    whereClause: Option[String] = None,
    previousEntities: List[NeoRequestEntity],
    nodeCounter: Int,
    edgeCounter: Int)
    (implicit val mf: Manifest[E]) extends NeoRequestEntity {

    val rangeId = "e" + edgeCounter

    val wherePart: Option[String] = whereClause.map(_.replace("_", rangeId))
    
    val typeName = mf.runtimeClass.getSimpleName

    val patternPart: String = s"[$rangeId:$typeName]"

    def node[N <: VkNode](whereClause: String = null)(implicit mf: Manifest[N]): Node[N] = {
        Node[N](if (whereClause == null) None else Some(whereClause),
            this :: previousEntities, nodeCounter, edgeCounter + 1)(mf)
    }

    def node[N <: VkNode](implicit mf: Manifest[N]): Node[N] = node[N](null: String)(mf)

    def extract(record: Record): E = {
        import net.liftweb.json._
        import net.liftweb.json.Extraction.decompose
        import net.liftweb.json.Serialization.{read, write}

        implicit val formats = Serialization.formats(NoTypeHints)

        val mutable = record.get(rangeId).asMap.asScala
        val map = Map(mutable.toSeq: _*)

        val json = decompose(map)
        json.extract[E](formats, mf)
    }
}

class Builder(entities: Seq[NeoRequestEntity]) {
    val pattern = entities.map(_.patternPart).mkString("-")
    val where = entities.flatMap(_.wherePart).mkString(" AND ")
    val ret = entities.map(_.rangeId).mkString(", ")

    val whereClause = if (where.isEmpty) "" else s" WHERE $where"

    val request = s"""MATCH $pattern$whereClause RETURN $ret"""

    def convert(neoResult: StatementResult) = {

        val records = neoResult.asScala.toSeq
        convert1(entities.toList, records)
    }

    def convert1(entities: List[NeoRequestEntity], records: Iterable[Record]): VkGraph = 
        entities match {
            case (n1: Node[_]) :: (e: Edge[_]) :: (n2: Node[_]) :: rest => {
                val edges = records.map(x => LUnDiEdge(n1.extract(x), n2.extract(x))(e.extract(x)))
                Graph.from(Nil, edges) ++ convert1(n2 :: rest, records)
            }
            case (n: Node[_]) :: Nil => Graph.from(records.map(n.extract _), Nil)
            case Nil => Graph()
        }    
}

object nutils {
    def node[N <: VkNode](whereClause: String = null)(implicit mf: Manifest[N]): Node[N] =
            Node[N](Option(whereClause))(mf)

    def node[N <: VkNode](implicit mf: Manifest[N]): Node[N] = node[N](null: String)(mf)

    def productToMap(cc: Product) = cc.getClass.getDeclaredFields.map( _.getName ).zip( cc.productIterator.to ).toMap

    def toNeoRecord(node: VkNode): String = {
        val m = productToMap(node)

        val kvs = m.map(x => {
            val value = x._2 match {
              case Some(x: String) => s"'$x'"
                case Some(x) => x.toString
                case None => "null"
                case x: String => s"'$x'"
                case x => x.toString
            }

            s"${x._1}:$value"
        })

        s"""(${node.identifier}:${node.getClass.getSimpleName} {${kvs.mkString(",")}})"""
    }

    def toNeoRequest(graph: VkGraph): String = {
      val nodesPart = graph.nodes.map(x => toNeoRecord(x.value)).toList

      val relPart = graph.edges.map(x => {
        val id1 = x.head.identifier
        val id2 = x.last.identifier
        s"($id1)-[:Friend]->($id2)"
      }).toList

      "CREATE " + (nodesPart ++ relPart).mkString(",\n")
    }
}