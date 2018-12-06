package datastructurs

object Exercise3_2 {
  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil // TODO : exception or monad
    case Cons(_, xs) => xs
  }

  // Exercise3.3
  def setHead[A](y: A, xs: List[A]): List[A] = xs match {
    case Nil => Nil // TODO : exception or monad
    case Cons(_, xs) => Cons(y, xs)
  }


  def main(args: Array[String]): Unit = {
    println(tail(List(2,1,2,3,9)))
  }
}
