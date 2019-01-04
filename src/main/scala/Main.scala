object Main extends App{
  val line = scala.io.StdIn.readLine

  def safeStringToInt(str: String): Option[Int] = {
    import scala.util.control.Exception._
    catching(classOf[NumberFormatException]) opt str.toInt
  }

  def hAndM(str:String): Array[Int] =
    str.split(":").map((s) => safeStringToInt(s).getOrElse(0))
  def wakeUpTime(xs: List[Int]): List[Int] = xs match {
    case h :: t  => if(h - 8 < 0) (h - 8) + 24 :: t else h - 8 :: t
  }

  val hm = hAndM(line)

  println(wakeUpTime(hm.toList).mkString(":"))
}
