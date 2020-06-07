
trait Monoid[A] {
  def zero: A
  def op: (A, A) => A
}

object MonoidAddition {

  implicit object Int extends Monoid[Int] {
    val zero = 0
    val op = (a: Int, b: Int) => a + b
  }

  implicit object Long extends Monoid[Long] {
    val zero = 0L
    val op = (a: Long, b: Long) => a + b
  }

  implicit object Double extends Monoid[Double] {
    val zero = 0.0
    val op = (a: Double, b: Double) => a + b
  }

  def fold[A : Monoid](xs: List[A]) = {
    val m = implicitly[Monoid[A]]
    xs.foldLeft(m.zero)(m.op)
  }

}

//trait Monad[A] {
//  def map[B](f: A => B): Monad[B] = {
//  }

trait OpCounter[S, +A] {
  //def map[B](f: A => B):

  def flatMap[B](f: A => S[B]) = {

}

//object Playing extends App {
//  println("playing")
//}
