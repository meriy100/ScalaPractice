import org.scalatest._

class Exercise2_2Test extends FunSuite with Matchers {
  test("testIsSorted") {
    Exercise2_2.isSorted(Array(1,2,3,4), (x: Int, y: Int) => { x <= y }) should be (true)
    Exercise2_2.isSorted(Array(1,1,3,4), (x: Int, y: Int) => { x <= y }) should be (true)
    Exercise2_2.isSorted(Array(2,1,3,4), (x: Int, y: Int) => { x <= y }) should be (false)

    Exercise2_2.isSorted(Array(2,1,3,4), (_: Int, _: Int) => { true }) should be (true)
  }
}
