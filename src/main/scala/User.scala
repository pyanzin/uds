package uds

import uds.graph._

case class User (
  id: Long,
  firstName: String,
  lastName: String,
  gender: Option[Boolean],
  birthDate: Option[String],
  cityId: Option[Int],
  photoUrl: String
) extends VkNode {
  val identifier: String = s"vku$id"
}

object User {
  val fullDateR = """(\d+).(\d+).(\d{4})""".r

  implicit class UserExt(u: User) {
    def birthYear = u.birthDate collect {
      case fullDateR(_, _, y) => y.toInt
    }

    private def sameName(a: String, b: String) = {
      def isRootsEquals(s1: String, s2: String) = {
        def getRoot(s: String) =
          if (s.length < 5) s else s.substring(0, s.length - 2)

        getRoot(s1) == getRoot(s2)
      }
      
      a.startsWith(b) ||
      b.startsWith(a) ||
      isRootsEquals(a,b)
    }

    def hasSameName(other: User) = sameName(u.lastName, other.lastName)
  }
}
