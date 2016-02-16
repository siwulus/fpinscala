package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
 def map[B](f: A => B): Either[E, B] = this match{
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match{
   case Right(a) => f(a)
   case Left(e) => Left(e)
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match{
   case Left(_) => b
   case Right(a) => Right(a)
 }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
  this flatMap((a) => b map(f(a, _)))
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight(Right(Nil):Either[E, List[B]])((a, b) => f(a).map2(b)(_::_))

  def traverse_1[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = es match{
    case h::t => f(h).map2(traverse_1(t)(f))(_::_)
    case Nil => Right(Nil)
  }


  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    es.foldRight(Right(Nil):Either[E, List[A]])((e, z) => e.map2(z)(_::_))

  def sequence_1[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    traverse(es)(a => a)

  def sequence_2[E,A](es: List[Either[E,A]]): Either[E,List[A]] = es match{
    case h::t => h.map2(sequence_2(t))(_::_)
    case Nil => Right(Nil)
  }



  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}