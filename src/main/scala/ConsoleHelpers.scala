package uds

package object ConsoleHelpers {
  def printAgeDistr(id: Long) = {
    import uds._, uds.graph._

    val vk = new Vk("", 0)

    val g = (new InitialLoader(vk))(id)

    val targetUser = g.nodes.collect(_.value match { case u: User if u.id == id => u}).head

    val analyzer = new FriendsAgeDistributionAnalyzer(targetUser)

    val d = analyzer.getProp(g)

    val absolute = 80
    val count = d.size.toDouble

    d.toList.sortBy(_._1).foreach(x => println(s"""${x._1}\t${"*"*(x._2 / count * absolute).toInt }"""))
  }
}