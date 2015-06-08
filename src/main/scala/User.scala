package uds

case class User(
  id: Long,
  firstName: String,
  lastName: String,
  gender: Option[Boolean],
  birthDate: Option[String],
  cityId: Option[Int]
)
