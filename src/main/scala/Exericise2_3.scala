object Exericise2_3 {
  def curry[A, B, C](f: (A, B) => C): A => (B=>C) = {
     a => b => f(a, b)
  }

  // Exercise2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  // Exercise2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    c => f(g(c))
  }

  def main(args: Array[String]): Unit = {
  }
}
