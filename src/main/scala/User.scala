package uds

import uds.graph._

case class User (
  id: Long,
  firstName: String,
  lastName: String,
  gender: Option[Boolean],
  birthDate: Option[String],
  cityId: Option[Int]
) extends VkNode

object User {
  implicit class UserExt(u: User) {
    val fullDateR = """(\d+).(\d+).(\d+)""".r

    def birthYear = u.birthDate map {
      case fullDateR(_, _, y) => Some(y.toInt)
      case _ => None
    }

    private def sameName(a: String, b: String) = {
      def isRootsEquals(s1: String, s2: String) = {
        def getRoot(s: String) =
          if (s.length > 5) s else s.substring(0, s.length - 2)

        getRoot(s1) == getRoot(s2)
      }
      
      a.startsWith(b) ||
      b.startsWith(a) ||
      isRootsEquals(a,b)
    }

    def hasSameName(other: User) = sameName(u.lastName, other.lastName)
  }
}
