package datastructurs

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs);
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  // Exercise3.1
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil // TODO : exception or monad
    case Cons(_, xs) => xs
  }

  // Exercise3.3
  def setHead[A](y: A, l: List[A]): List[A] = l match {
    case Nil => Nil // TODO : exception or monad
    case Cons(_, xs) => Cons(y, xs)
  }

  // Exercise3.3
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) =>
      if(n > 1) drop(xs, n - 1)
      else xs
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
