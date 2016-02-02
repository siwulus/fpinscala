package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l,r) => size(l) + size(r) + 1
    }
  }

  def maximum(t: Tree[Int]): Int = t match {
      case Leaf(v) => v
      case Branch(l,r) => maximum(l).max(maximum(r))
  }

  def fold[A, B](t: Tree[A], z: A => B)(f: (B,B) => B): B = t match {
    case Leaf(v) => z(v)
    case Branch(l, r) => f(fold(l, z)(f), fold(r, z)(f))
  }


}