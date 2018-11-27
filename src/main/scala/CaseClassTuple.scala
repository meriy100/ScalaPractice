object CaseClassTuple {
  def get() = {
    val name = new Name("田中", "太郎")
    (Human(name, 15), name)
  }

  def main(args: Array[String]){
    val tuple = get()
    printf("fullName_1=%s \n", tuple._1.name.fullName)
    printf("fullName_2=%s \n", tuple._2.fullName)
  }
}

class Name(firstName:String, lastName:String) {
  def fullName() = firstName + " " + lastName
}

case class Human(name: Name, age: Int)
