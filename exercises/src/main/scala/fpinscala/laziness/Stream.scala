package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  def toListRecursive: List[A] =
    this.foldRight(List[A]())((a, z) => a::z)

  def toListRecursive_1: List[A] = this match {
    case Cons(h, t) => h()::t().toListRecursive_1
    case _ => List[A]()
  }

  def toList: List[A] = {
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h()::acc)
      case _ => acc
    }
    go(this, List[A]()).reverse
  }

  def toList_1: List[A] = {
    val acc = collection.mutable.ListBuffer[A]()
    def go(s:Stream[A]): List[A] = s match {
      case Cons(h,t) => acc += h(); go(t())
      case _ => acc.toList
    }
    go(this)
  }

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = this match {
    case Cons(h,t) if (n > 0) => cons(h(), t().take(n-1))
    case _ => Empty
  }


  def drop(n: Int): Stream[A] = this match {
    case Cons(h,t) if (n > 0) => t().drop(n -1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] =
    this.foldRight(Empty:Stream[A])((a, s) => if(p(a)) cons(a, s) else empty)

  def takeWhile_1(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if(p(h())) => cons(h(), t().takeWhile(p))
    case  _ => Empty
  }


  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a,z) => p(a) && z)

  def forAll_1(p: A => Boolean): Boolean = this match{
    case Cons(h, t) => p(h()) && t().forAll_1(p)
    case _ => true
  }


  def headOption: Option[A] = this.foldRight(None:Option[A])((h, _) =>Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    this.foldRight(Empty:Stream[B])((h,t) => cons(f(h), t))

    def filter(f: A => Boolean):Stream[A] =
    this.foldRight(Empty:Stream[A])((h,t) => if (f(h)) cons(h, t) else t)

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h,t) => cons(h,t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty:Stream[B])((h,t) => f(h).append(t))

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this)(s => s match {
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    })

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((n,this))(s => s match {
      case (n, Cons(h, t)) if n > 0 => Some(h(), (n - 1, t()))
      case _ => None
    })

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this)(s => s match {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    })

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this,s))((ss) => ss match{
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    })

  def zipAllWith[B, C](s: Stream[B])(f: (Option[A],Option[B]) => C): Stream[C] =
    unfold((this,s))((ss) => ss match{
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())), (t1(), t2()))
      case (Empty, Cons(h2, t2)) => Some(f(None, Some(h2())), (Empty, t2()))
      case (Cons(h1, t1), Empty) => Some(f(Some(h1()), None), (t1(), Empty))
      case _ => None
    })

  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] =
    this.zipAllWith(s)((_,_))

//  def hasSubsequence(s: Stream[A]): Boolean =
//    this.foldRight((true, s))((a, z) => {})

  def startsWith[B](s: Stream[B]): Boolean = (this, s) match {
    case (Cons(h1, t1), Cons(h2, t2)) => if (h1() == h2()) t1().startsWith(t2()) else false
    case (Empty, Cons(_,_)) => false
    case (Cons(_,_), Empty) => true
    case (Empty, Empty) => true
  }

  def startsWithViaZipAll[B](s: Stream[B]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty).forAll{case (a,b) => a == b}

  def tails: Stream[Stream[A]] =
    unfold(this)(s => s match{
      case Empty => None
      case s => Some((s, s.drop(1)))
    })


}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    lazy val s: Stream[A] = Cons(() => a, () => s)
    s
  }

  def constant_1[A](a: A): Stream[A] =
    cons(a, constant_1(a))

  def fib():Stream[Int] = {
    def go(m: Int, n: Int):Stream[Int] = {
      cons(m, go(n, m + n))
    }
    go(0,1)
  }

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).map((x: (A, S)) => cons(x._1, unfold(x._2)(f))).getOrElse(empty)

  def fib_1():Stream[Int] =
    unfold[Int,(Int, Int)]((0,1))(z => Some(z._1,(z._2, z._1 + z._2)))

  def from_1(n: Int): Stream[Int] =
    unfold(n)(x => Some(x, x + 1))

  def constant_2[A](a: A): Stream[A] =
    unfold(a)(a => Some(a,a))

  val ones_1 = constant_2(1)
}

object StreamMain{
  def main(args: Array[String]) {
    println(Stream(1,2,3,4,5).tails.map(_.toList).toList)
//    println(Stream(1,2,3,4).startsWithViaZipAll(Stream(1,2)))
//    println(Stream(1,2,3,4).startsWithViaZipAll(Stream(2)))
//    println(Stream(1,2,3,4).startsWithViaZipAll(empty))
//    println(empty.startsWithViaZipAll(empty))
//    println(empty.startsWithViaZipAll(Stream(1,2,3)))
//    println(Stream(1,2,3,4).startsWith(Stream(1,2,3,4)))
    //println(fib().takeViaUnfold(7).toList)
    //println(empty.takeViaUnfold(7).toList)
    //println(fib().takeWhileViaUnfold(_ < 9).toList)
    //println(fib().mapViaUnfold(_ + 1).takeViaUnfold(7).toList)
    //println(empty[Int].mapViaUnfold(_ + 1).takeViaUnfold(7).toList)
//    println(fib_1().take(7).toList)
//    println(from(5).take(7).toList)
//    println(from_1(5).take(7).toList)
//    println(constant(2).take(7).toList)
//    println(constant_2(2).take(7).toList)
//    println(ones_1.take(7).toList)
  }
}
