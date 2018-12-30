package datastructurs

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](branch: Tree[A]): Int = branch match {
    case Leaf(_) => 1
    case Branch(x, y) => size(x) + size(y) + 1
  }

  def maximum(branch: Tree[Int]):Int = branch match {
    case Leaf(x) => x
    case Branch(x, y) => maximum(x) max maximum(y)
  }

  def depth[A](branch: Tree[A]): Int = branch match {
    case Leaf(_) => 0
    case Branch(x, y) => 1 + (depth(x) max depth(y))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(x, y) => Branch(map(x)(f), map(y)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def size2[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(_ + _ + 1)

  def maximum2(t: Tree[Int]): Int =
    fold(t)(v => v)((x:Int, y:Int) => x max y)

  def depth2[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((x:Int, y:Int) => 1 + (x max y))

  def map2[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)((v) => Leaf(f(v)):Tree[B])(Branch(_, _))
}

