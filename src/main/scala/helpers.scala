package uds

package object Helpers {
  implicit class TraversableExt[T](xs: Traversable[T]) {
    def distribution[K](f: T => K): Map[K, Int] =
      xs.groupBy(f).mapValues { _.size }
  }
}
