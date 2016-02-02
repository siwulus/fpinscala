package fpinscala.datastructures

sealed trait Tree[+A]{
  def size(): Int
}
case class Leaf[A](value: A) extends Tree[A]{
  def size() = 1
}

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]{
  def size() = left.size() + right.size() + 1
}


object Tree {
  def size[A](t: Tree[A]): Int = t match {
      case l: Leaf => 1
      case b: Branch => size(b.left) + size(b.right) + 1
  }



}