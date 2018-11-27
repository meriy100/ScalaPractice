object Exercise2_1 {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(m: Int, beforeFirst: Int, beforeSecond: Int): Int = {
      if (m < n) go(m + 1, beforeSecond, beforeFirst + beforeSecond)
      else beforeFirst + beforeSecond
    }
    if (n == 1) 0
    else if (n == 2) 1
    else go(3, 0, 1)
  }

  def main(args: Array[String]): Unit ={
    def loop(n: Int, f: Int => Unit, g: Int => Int): Unit = {
      if (1 < n) {
        f(g(n))
        loop(n - 1, f, g)
      } else {
        f(g(n))
      }
    }

    loop(10, println, fib)
  }
}
