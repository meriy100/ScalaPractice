object Exercise2_2 {

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def any(target: A, as: Array[A]): Boolean = {
      if (as.length == 0) true
      else if (ordered(target, as(0))) any(target, as.slice(1, as.length))
      else false
    }

    any(as(0), as.slice(1, as.length))
  }

  def main(args: Array[String]): Unit = {
    if (isSorted(Array(2,2,3,4), (x: Int, y: Int) => { x <= y })) println("ok")
    else println("NG")
  }
}
