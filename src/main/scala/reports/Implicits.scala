package uds.reports

package object Implicits {
  trait TextVisualizable[A] {
    def toText(x: A): Seq[String]
  }

  def toTxt[A: TextVisualizable](x: A) = implicitly[TextVisualizable[A]].toText(x)

  implicit val intMapVis = new TextVisualizable[Map[Int, Int]] {
    def toText(x: Map[Int, Int]): Seq[String] = {
      x.toList.sortBy(_._1).map(x => s"""${x._1}\t${"*"*x._2}""")
    }
  }
}