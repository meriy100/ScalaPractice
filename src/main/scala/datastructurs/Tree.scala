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
}

