package datastructurs

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def foldLeft2[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(as, (b:B) => b)((a,g) => b => g(f(b,a)))(z)

  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b,a) => f(a,b))

  def join[A](aas: List[List[A]]): List[A] =
    foldLeft(reverse(aas), Nil:List[A])((ys, xs) => append2(xs, ys))

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(reverse(a1), a2)((t, h) => Cons(h, t))

  def reverse[A](as: List[A]):List[A] = {
    foldLeft2(as, Nil:List[A])((xs, x) => Cons(x, xs))
  }

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)
  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)
  def length2[A](as: List[A]) =
    foldLeft(as, 0)((x, _) => 1 + x)

  def length[A](as: List[A]) =
    foldRight(as, 0)((_, y) => 1 + y)

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)(_ + _)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs);
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  // Exercise3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil // TODO : exception or monad
    case Cons(_, xs) => xs
  }

  // Exercise3.3
  def setHead[A](y: A, l: List[A]): List[A] = l match {
    case Nil => Nil // TODO : exception or monad
    case Cons(_, xs) => Cons(y, xs)
  }

  // Exercise3.4
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) =>
      if(n > 1) drop(xs, n - 1)
      else xs
  }

  // Exercise3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) =>
      if(f(x)) dropWhile(xs, f)
      else Cons(x, xs)
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
