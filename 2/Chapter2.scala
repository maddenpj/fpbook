
sealed trait Tree[+A]
//case class Empty extends Tree[Nothing]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size(x: Tree[_]): Int = x match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maxInt(tree:  Tree[Int]): Int = {
    def go(curr: Int, x: Tree[Int]): Int = x match {
      case Leaf(n) => curr max n
      case Branch(l, r) => go(curr, l) max go(curr, r)
    }

    go(Int.MinValue, tree)
  }

  def depth(tree: Tree[_]): Int = {
    def go(curr: Int, x: Tree[_]): Int = x match {
      case Leaf(_) => curr + 1
      case Branch(l, r) => go(1 + curr, l) max go(1 + curr, r)
    }
    go(1, tree)
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](tree: Tree[A], acc: B)(combOp: (B, B) => B)(f: (A, B) => B): B = {
    def go(a: B, t: Tree[A]): B = t match {
      case Leaf(x) => f(x, a)
      case Branch(l, r) => combOp(go(a, l), go(a, r))
    }
    go(acc, tree)
  }

  //def empty[A]: Tree[A] =

  def sizeF(x: Tree[_]): Int = fold(x, 0)(_ + _ + 1) { (_, acc) => 1 }

  def maxIntF(x: Tree[Int]) = fold(x, Int.MinValue)(_ max _)(_ max _)

  def depthF(x: Tree[Int]) = fold(x, 1)(_ max _) { (_, acc) => acc + 1 }

  //def mapF[A, B](x: Tree[A])(f: A => B) = fold(x,)({ (l, r) => Branch(l, r) }) { a => Leaf(f(a)) }
  //
  def test = "this is a test string"

  def example: Tree[Int] =
    Branch(
      Branch(
        Leaf(4),
        Branch(
          Leaf(9),
          Leaf(42)
        )
      ),
      Branch(
        Branch(
          Branch(
            Leaf(19),
            Leaf(3)
          ),
          Leaf(5)
        ),
        Leaf(-10)
      )
  )
}


object Chapter2 {

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = (b: B) => f(a, b)

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => partial1(a, f)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))

  def main(args: Array[String]) = println("Hello World")
}
