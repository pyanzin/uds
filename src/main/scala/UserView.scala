package uds

trait View[T] {
	def convert: T
}

case class UserView (
	uid: Int,
	first_name: String,
	last_name: String,
	sex: Int,
	bdate: Some[String],
	city: Some[Int],
	photo_100: String,
	deactivated: Some[String]
) extends View[User] {
	def convert = User(
		uid,
		first_name,
		last_name,
		sex match {
			case 0 => None 
			case 1 => Some(false)
			case 2 => Some(true)
		},
		bdate,
		city,
		photo_100
	)
}