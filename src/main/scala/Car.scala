object Car {
  def main(args: Array[String]) {
    val car = new Car("blue")
    car.run
  }

  private def output(message: String) { println(message) }
}

class Car(color: String) {
  def run = { Car.output("Start!!") }
}
