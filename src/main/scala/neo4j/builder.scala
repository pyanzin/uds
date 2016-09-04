package uds.neo4j

trait NeoRequestEntity {
    def rangeId: String
    def wherePart: Option[String]    
    def typeName: String
    def patternPart: String
}

case class Node[N](
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

    def node[N](whereClause: String = null)(implicit mf: Manifest[N]): Node[N] = {
        Node[N](if (whereClause == null) None else Some(whereClause),
            this :: previousEntities, nodeCounter, edgeCounter + 1)(mf)
    }

    def node[N](implicit mf: Manifest[N]): Node[N] = node[N](null.asInstanceOf[String])(mf)
}

class Builder(entities: Seq[NeoRequestEntity]) {
    val pattern = entities.map(_.patternPart).mkString("-")
    val where = entities.flatMap(_.wherePart).mkString(" && ")
    val ret = entities.map(_.rangeId).mkString(", ")

    val whereClause = if (where.isEmpty) "" else s" WHERE $where"

    val request = s"""MATCH $pattern$whereClause RETURN $ret"""
}

object util {
    def node[N](whereClause: String = null)(implicit mf: Manifest[N]): Node[N] =
            Node[N](if (whereClause == null) None else Some(whereClause), Nil, 0, 0)(mf)

    def node[N](implicit mf: Manifest[N]): Node[N] = node[N](null.asInstanceOf[String])(mf)
}