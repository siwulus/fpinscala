package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
      case Some(a) => Some(f(a))
      case None => None
  }

  def getOrElse[B>:A](default: => B): B = this match{
      case Some(a) => a
      case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case Some(a) => f(a)
      case None => None
  }

  def flatMap_1[B](f: A => Option[B]): Option[B] =
    this.map(f).getOrElse(None)

  def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
    case Some(a) => this
    case None => ob
  }

  def filter(f: A => Boolean): Option[A] = this match {
      case Some(a) if (f(a)) => this
      case _ => None
  }

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
      a.flatMap(aa => b.map(bb => f(aa,bb)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
      case h::t => map2(h, sequence(t))((hh, aa) => hh::aa)
      case Nil => Some(Nil)
  }

  def sequence_1[A](a: List[Option[A]]): Option[List[A]] = a match {
    case h::t => h.flatMap (hh => sequence_1(t).map(hh::_))
    case Nil => Some(Nil)
  }


  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match{
      case h::t => traverse(t)(f).flatMap((l => f(h).map(_::l)))
      case Nil => Some(Nil)
  }


  def traverse_1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match{
    case h::t => map2(f(h), traverse_1(t)(f))(_::_)
    case Nil => Some(Nil)
  }

}

object OptionRun{

  import Option._

  def main(args: Array[String]) {
    println("sequence")
    println(sequence(Nil))
    println(sequence(List()))
    println(sequence(List(Some(1))))
    println(sequence(List(None)))
    println(sequence(List(Some(1),Some(2),Some(3),Some(4),Some(5))))
    println(sequence(List(Some(1),Some(2),None,Some(4),Some(5))))
    println("sequence_1")
    println(sequence_1(Nil))
    println(sequence_1(List()))
    println(sequence_1(List(Some(1))))
    println(sequence_1(List(None)))
    println(sequence_1(List(Some(1),Some(2),Some(3),Some(4),Some(5))))
    println(sequence_1(List(Some(1),Some(2),None,Some(4),Some(5))))
  }
}