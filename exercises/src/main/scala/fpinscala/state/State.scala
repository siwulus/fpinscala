package fpinscala.state

import fpinscala.state.RNG._

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    if(i < 0) (-(i + 1), r) else (i, r)
  }

  val nonNegativeIntRand: Rand[Int] = nonNegativeInt

  def double(rng: RNG): (Double, RNG) = {
    val (i,r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble  + 1), r)
  }

  def doubleViaMap: Rand[Double] = map(nonNegativeInt)(_/(Int.MaxValue.toDouble  + 1))

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r1) = nonNegativeInt(rng)
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def intDoubleViaMap2: Rand[(Int, Double)] = map2(nonNegativeInt, doubleViaMap)((_,_))


  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def doubleIntViaMap2: Rand[(Double,Int)] = map2(doubleViaMap, nonNegativeInt)((_,_))


  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3),r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(n: Int, r: (Int, RNG), acc:List[Int]):(List[Int], RNG) = {
      if (n > 0)
        go(n - 1, RNG.nonNegativeInt(r._2), r._1::acc)
      else
        (acc, r._2)
    }
    go(count, RNG.nonNegativeInt(rng), List())
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a,b), r2)
    }

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???


  // List(double, double, double) => double(List(1,2))
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rnd => {
    fs.foldRight((List[A](), rnd))((rand: Rand[A], z:(List[A], RNG)) => z match {
      case (l, r1) =>
        val (a, r2) = rand(r1)
        (l:::List(a), r2)
    })
  }

  def sequenceViaUnit[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((ra, z) => map2(ra, z)(_::_))



  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rnd => {
    val (a, r) = f(rnd)
    g(a)(r)
  }

}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = ???
    //flatMap()

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State(s => {
      val (a, s1) = run(s)
      val (b, s2) = sb.run(s1)
      (f(a,b), s2)
    })

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}

object StateMain{
  def main(args: Array[String]) {
    println(sequence(List.fill(5)(nonNegativeIntRand))(Simple(1)))
    println(sequenceViaUnit(List.fill(5)(nonNegativeIntRand))(Simple(1)))

  }
}