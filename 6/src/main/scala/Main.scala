import scala.util.Random

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng2) = rng.nextInt
    n match {
      case Int.MinValue => nonNegativeInt(rng2)
      case x if x < 0 => (-x, rng2)
      case _ => (n, rng2)
    }
  }

  //def double(rng: RNG): (Double, RNG) = {
  //  val (n, rng2) = nonNegativeInt(rng)
  //  (n.toDouble / Int.MaxValue, rng2)
  //}

  def double: Rand[Double] = map(int) { n =>
    n.toDouble / Int.MaxValue
  }
  //def double(rng: RNG): (Double, RNG) = {
}

//case class State[S,+A](run: S => (A, S)) {
//  def map[B](f: A => B): State[S, B] = State(state => {
//    val (a, s2) = run(state)
//    (f(a), s2)
//  })
//  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State(state => {
//    val (a, s2) = run(state)
//    val (b, s3) = sb.run(s2)
//    (f(a, b), s3)
//  })
//  //def flatMap[B](f: A => State[S, B]): State[S, B] = State(state => {
//  //  f(
//  //}
//  //  sys.error("todo")
//
//  //def unit
//
//  //def sequence
//}

import State._

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(state => (a, state))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    def go(s: S, actions: List[State[S,A]], acc: List[A]): (List[A],S) =
      actions match {
        case Nil => (acc.reverse,s)
        case h :: t => h.run(s) match { case (a,s2) => go(s2, t, a :: acc) }
      }
    State((s: S) => go(s,sas,List()))
  }

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {
  def transition(x: Input)(m: Machine) = x match {
    case Coin if(m.locked && m.candies > 0) => Machine(false, m.candies, m.coins + 1)
    case Turn if(!m.locked && m.candies > 0) => Machine(true, m.candies - 1, m.coins)
    case _ => m
  }

  val string = "This is a string"
}

object Candy {
  //def simulate(inputs: List[Input]): State[Machine, (Int, Int)] = {
  //  val transitions = inputs.map(i => State(Machine.transition(i)))
  //  //State.sequence(State.set(initial) :: transitions)
  //}
}

object Main extends App {
  println("")
}
