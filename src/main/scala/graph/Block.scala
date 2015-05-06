package uds.graph

trait Block[A, B] extends Function[A, B] {
  def apply(x: A): B
}
