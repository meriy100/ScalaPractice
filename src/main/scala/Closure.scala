object Closure{
  def main(args: Array[String]){
    val candidates = List(1, 2, 3, 4, 5, 6, 8)

    val filter = (predicate :Int => Boolean) => {
      (candidates :List[Int]) => {
        for(x <- candidates; if predicate(x)) yield x
      }
    }

    filter((x:Int) => x % 3  == 0)(candidates).foreach(println)
  }
}
