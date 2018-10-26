class User(val id: Long, val firstname: String, val lastname: String, val fullName:FullName = new FullName) {
  def getFullName() = this.fullName.get(firstname, lastname)
}

class FullName() {
  def get(firstName:String, lastName:String) = firstName + " " + lastName
}

class FullNameJapanese() extends FullName {
  override def get(firstName:String, lastName:String) = lastName + " " + firstName
}

object Main {
  def main(args: Array[String]){
    val users = List(
      new User(1, "Jone", "Pole"),
      new User(2, "太郎", "山田", new FullNameJapanese),
    )
    users.foreach(user => { println(user.getFullName()) })
  }
}
