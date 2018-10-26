object FunctionExample{
  def main(args: Array[String]){
    val calculate = (x: Int) => x + 5 : Int

    printf("y=%s \n", calculate(1))
    printf("y=%s \n", calculate(2))
  }
}
