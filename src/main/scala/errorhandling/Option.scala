package errorhandling

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)) getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    if (map(f) getOrElse(false)) this
    else None
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int):Int = {
    val y: Int = throw new Exception("fail!")
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 }
  }

  def failingFn2(i: Int):Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int)
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Double =
    if (xs.isEmpty)
      throw new ArithmeticException("mean of empty list!")
    else xs.sum / xs.length

  def mean2(xs: Seq[Double], onEmpty: Double): Double =
    if (xs.isEmpty) onEmpty
    else xs.sum / xs.length

  def mean3(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean3(xs) flatMap (m => mean3(xs map (x => math.pow(x - m, 2))))
  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (av => b map (bv => f(av, bv)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.reverse.foldLeft(Some(Nil):Option[List[A]])((xs, x) => map2(x, xs)(_ :: _))
}
