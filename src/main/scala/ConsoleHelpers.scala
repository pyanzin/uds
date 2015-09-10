package uds

package object ConsoleHelpers {
  def printAgeDistr(id: Long) = {
    import uds._, uds.graph._

    val vk = new Vk("", 0)

    val g = (new InitialLoader(vk))(id)

    val targetUser = g.nodes.collect(_.value match { case u: User if u.id == id => u}).head

    val analyzer = new FriendsAgeDistributionAnalyzer(targetUser)

    val d = analyzer.getProp(g)

    val absolute = 30
    val count = d.size.toDouble

    d.toList.sortBy(_._1).foreach(x => println(s"""${x._1}\t${"*"*(x._2 / count * absolute).toInt }"""))
  }

  def getMode[T](dist: Map[T, Int]): T = dist.maxBy(_._2)._1

  def getMean(dist: Map[Int, Int]) = dist.map(_._1).sum / dist.size

  def getMedian(dist: Map[Int, Int]) = {
  val half = dist.values.sum / 2
    dist.toList.sortBy(_._2).foldRight((0, 0))((x, z) =>
      if (z._2 > half)
        z
      else
        (x._1, z._2 + x._2)
    )._1
  }

  def fillGaps(dist: Map[Int, Int]) = {
    val min = dist.values.min
    val max = dist.values.max

    min to max map { x =>
      (x, dist.getOrElse(x, 0))
    }
  }
}