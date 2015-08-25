package uds

package object Helpers {
  implicit class TraversableExt[T](xs: Traversable[T]) {

  	/** Calculate distribution of elements by K **/
    def distribution[K](f: T => K): Map[K, Int] =
      xs.groupBy(f).mapValues { _.size }
  }

  implicit class ListExt[T](xs: List[T]) {

  	/** Groups elements of list while adjacent elements satisfy f() **/
    def groupWhile[A](f: (T, T) => Boolean): List[List[T]] = xs match {
      case Nil => Nil
      case x :: xs => xs.foldRight(List(List(x)))((a, acc) => acc match {
        case (b :: xs) :: xss if f(a, b) => (a :: b :: xs) :: xss
        case xss => List(a) :: xss
      })
    }
  }
}
