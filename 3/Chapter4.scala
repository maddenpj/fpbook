package Chapter4


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this.map(f).getOrElse(None)

  // Some(1).getOrElse(4) == 1
  // None.getOrElse(4) == 4
  // Some(1).orElse(Some(5)) == Some(1)
  // None.orElse(Some(5)) == Some(5)
  //if(this.isDefined) this else ob
  def orElse[B >: A](ob: => Option[B]): Option[B] = this.map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = this.flatMap { x =>
    if(f(x)) this else None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap { m =>
    mean(xs.map(x => math.pow(x - m, 2)))
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap { aa =>
      b.map { bb =>
        f(aa, bb)
      }
    }

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(List[A]())) { (x, xs) =>
      map2(x, xs)(_ :: _)
      // x.flatMap { xx =>
      //   xs.map { xss =>
      //     xx :: xss
      //   }
      // }
    }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(identity)

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(List.empty[B])) { (x, xs) =>
      map2(f(x), xs)(_ :: _)
    }

}
