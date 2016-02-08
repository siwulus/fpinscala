package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
      case Nil => sys.error("Nil list")
      case Cons(_, t) => t
    }

  def setHead[A](l: List[A], h: A): List[A] = l match {
        case Nil => sys.error("Nil list")
        case Cons(_, t) => Cons(h, t)
  }


  def drop[A](l: List[A], n: Int): List[A] = {
    l match {
        case Nil => Nil
        case Cons(_,xs) => if (n <= 0) l else drop(xs, n - 1)

    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
        case Nil => Nil
        case Cons(x,xs) => if (f(x)) dropWhile(xs, f) else l
    }
  }


  def init[A](l: List[A]): List[A] = l match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(x,xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int = foldRight(l,0)((_,b) => b + 1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
      case Nil => z
      case Cons(x,xs) => foldLeft(xs, f(z, x))(f)
  }

  def sumFL(as: List[Int]): Int = foldLeft(as, 0)(_+_)

  def productFL(as: List[Int]): Int = foldLeft(as, 1)(_*_)

  def lengthFL[A](as: List[A]): Int = foldLeft(as, 0)((acc,_) => acc + 1)

  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((rs, a) => Cons(a, rs))

  def foldLeftAsFR[A,B](l: List[A], z: B)(f: (B, A) => B): B = foldRight(List.reverse(l), z)((a,b) => f(b,a))

  def foldRightAsFL[A,B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((b,a) => f(a,b))

  def append2[A](a1: List[A], a2: List[A]): List[A] = foldLeft(reverse(a1), a2)((l2, a) => Cons(a, l2))
  def append3[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((a,l) => Cons(a, l))

  def concat[A](l: List[List[A]]): List[A] = foldRightAsFL(l, Nil: List[A])(append3)

  def add1(il: List[Int]): List[Int] = foldRightAsFL(il, Nil: List[Int])((i,l) => Cons(i + 1, l))

  def doubleToString(dl: List[Double]): List[String] = foldRightAsFL(dl, Nil: List[String])((d,l) => Cons(d.toString, l))

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRightAsFL(l, Nil: List[B])((a, b) => Cons(f(a), b))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRightAsFL(as, Nil: List[A])((a, fl) => if (f(a)) Cons(a, fl) else fl)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = List.concat(List.map(as)(f))

  def filter2[A](as: List[A])(f: A => Boolean): List[A]= flatMap(as)(a => if(f(a)) List(a) else Nil)

  def zip[A](as: List[A], bl: List[A])(z: (A, A) => A): List[A] = (as, bl) match {
    case (Cons(ha, ta), Cons(hb, tb)) => Cons(z(ha, hb), zip(ta, tb)(z))
    case _ => Nil
  }

}

object Main{

  def main(args: Array[String]) {
    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }
    println(x)

    println
    println("setHead")
    println(List.setHead(List(1,2,3), None))
    println(List.setHead(List(1,2,3), 2))
    println(List.setHead(List(1), 2))

    println
    println("drop")
    println(List.drop(Nil, 0)) // Nil
    println(List.drop(Nil, 3)) // Nil
    println(List.drop(List(1,2,3,4,5), 0)) // List(1,2,3,4,5)
    println(List.drop(List(1,2,3,4,5), 1)) // List(2,3,4,5)
    println(List.drop(List(1,2,3,4,5), 3)) // List(4,5)
    println(List.drop(List(1,2,3,4,5), 7)) // Nil


    println
    println("dropWhile")
    println(List.dropWhile(Nil, (x: Int) => x < 3)) // Nil
    println(List.dropWhile(List(1,2,3,4), (x: Int) => true)) // Nil
    println(List.dropWhile(List(1,2,3,4,5), (x: Int) => x < -1)) // List(1,2,3,4,5)
    println(List.dropWhile(List(1,2,3,4,5), (x: Int) => x <= 1)) // List(2,3,4,5)
    println(List.dropWhile(List(1,2,3,4,5), (x: Int) => x <= 3)) // List(4,5)

    println
    println("init")
    println(List.init(List(1))) // Nil
    println(List.init(List(1,2,3,4,5))) // List(1,2,3,4,)3)) // List(4,5)

    println
    println("reverse")
    println(List.reverse(List(Nil))) // Nil
    println(List.reverse(List(1))) // 1
    println(List.reverse(List(1,2))) // 2,1
    println(List.reverse(List(1,2,3))) // 3,2,1

    println
    println("append2")
    println(List.append2(Nil, Nil)) // Nil
    println(List.append2(Nil, List(1))) // 1
    println(List.append2(List(1), Nil)) // 1
    println(List.append2(List(1,2), List(3))) // 1,2,3
    println(List.append2(List(1,2,3), List(4,5,6))) // 1,2,3,4,5,6

    println
    println("append3")
    println(List.append3(Nil, Nil)) // Nil
    println(List.append3(Nil, List(1))) // 1
    println(List.append3(List(1), Nil)) // 1
    println(List.append3(List(1,2), List(3))) // 1,2,3
    println(List.append3(List(1,2,3), List(4,5,6))) // 1,2,3,4,5,6


    println
    println("concat")
    println(List.concat(Nil)) // Nil
    println(List.concat(List(Nil))) // Nil
    println(List.concat(List(List(1), List(2,3), List(4,5,6)))) // Nil

    println
    println("filter")
    println(List.filter(Nil)(_=>true)) // Nil
    println(List.filter(Nil)(_=>false)) // Nil
    println(List.filter(List(1,2,3))(_=>false)) // Nil
    println(List.filter(List(1,2,3))(_=>true)) // 1,2,3
    println(List.filter(List(1,2,3,4))(x=>x % 2 == 0 )) // 2,4

    println
    println("filter2")
    println(List.filter2(Nil)(_=>true)) // Nil
    println(List.filter2(Nil)(_=>false)) // Nil
    println(List.filter2(List(1,2,3))(_=>false)) // Nil
    println(List.filter2(List(1,2,3))(_=>true)) // 1,2,3
    println(List.filter2(List(1,2,3,4))(x=>x % 2 == 0 )) // 2,4


    println
    println("flatMap")
    println(List.flatMap(List(1,2,3,4,5,6))(b => List(b.toString, "toString")))


    println
    println("zip")
    println(List.zip(List(1), List(2))(_ + _))
    println(List.zip(List(1,2,3,4), List(5,6,7,8))(_ + _))
    println(List.zip(List(1,2,3,4), List(5,6,7,8,9,10))(_ + _))
    println(List.zip(List(1,2,3,4,5,6,7), List(5,6,7,8))(_ + _))

  }
}