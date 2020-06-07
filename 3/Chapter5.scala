package Chapter5



import Stream._
trait Stream[+A] {
  def toList: List[A] = {
    def go(a: Stream[A]): List[A] = a match {
      case Empty => List.empty[A]
      case Cons(h, t) => h() :: go(t())
    }
    go(this)
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => empty
    case Cons(h, t) => cons(h(), take(n -1))
  }
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


  def test: Stream[Int] = cons(1,
    cons(2,
      cons(3, empty)
      )
    )
}
