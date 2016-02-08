package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def fold[A, B](t: Tree[A])(z: A => B)(f: (B,B) => B): B = t match {
    case Leaf(v) => z(v)
    case Branch(l, r) => f(fold(l)(z)(f), fold(r)(z)(f))
  }

  def size[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l,r) => size(l) + size(r) + 1
    }
  }

  def size2[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ + _ + 1)

  def maximum(t: Tree[Int]): Int = t match {
      case Leaf(v) => v
      case Branch(l,r) => maximum(l).max(maximum(r))
  }

  def maximum2(t: Tree[Int]): Int = fold(t)(a => a)(_ max _)

  def depth[A](t: Tree[A]): Int = fold(t)(_ => 1)(_.max(_) + 1)

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))
}

object MainTree{

  def main(args: Array[String]): Unit = {
    println("size")
    println(Tree.size(Leaf(20))) //1
    println(Tree.size(Branch(Leaf(1), Leaf(3))))
    println(Tree.size(Branch(Branch(Branch(Leaf(1), Leaf(3)), Leaf(30)), Leaf(20))))
    println
    println("size2")
    println(Tree.size2(Leaf(20))) //1
    println(Tree.size2(Branch(Leaf(1), Leaf(3))))
    println(Tree.size2(Branch(Branch(Branch(Leaf(1), Leaf(3)), Leaf(30)), Leaf(20))))

    println
    println("maximum")
    println(Tree.maximum(Leaf(20))) //1
    println(Tree.maximum(Branch(Leaf(1), Leaf(3))))
    println(Tree.maximum(Branch(Branch(Branch(Leaf(1), Leaf(3)), Leaf(30)), Leaf(20))))

    println("maximum2")
    println(Tree.maximum2(Leaf(20))) //1
    println(Tree.maximum2(Branch(Leaf(1), Leaf(3))))
    println(Tree.maximum2(Branch(Branch(Branch(Leaf(1), Leaf(3)), Leaf(30)), Leaf(20))))

    println("depth")
    println(Tree.depth(Leaf(20))) //1
    println(Tree.depth(Branch(Leaf(1), Leaf(3)))) //2
    println(Tree.depth(Branch(Branch(Leaf(3), Leaf(30)), Leaf(20)))) //3
    println(Tree.depth(Branch(Branch(Branch(Leaf(1), Leaf(3)), Leaf(30)), Leaf(20)))) //4

    println("map")
    println(Tree.map(Branch(Branch(Branch(Leaf(1), Leaf(3)), Leaf(30)), Leaf(20)))(a => a.toString + "Int"))
  }
}
