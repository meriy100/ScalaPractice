object CarExtends {
  def main(args: Array[String]): Unit = {
    val car = new Car("blue")
    output(car)
    car.run

    val truck = new Truck("Silver")
    output(truck)
    truck.run
  }

  def output(car: Car):Unit=println("Color is " + car.color + ".")

  class Car(val color :String) {
    def run = println("Start!!")
  }

  class Truck(color:String) extends Car(color) {
    override def run: Unit = println("Truck start!!")
  }
}
