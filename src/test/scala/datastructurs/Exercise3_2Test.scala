package datastructurs

import org.scalatest._

class Exercise3_2Test extends FunSuite with Matchers {

  test("testTail") {
    Exercise3_2.tail(Nil) should equal(Nil)
    Exercise3_2.tail(List(1,2,3,4,5)) should equal(List(2,3,4,5))
    Exercise3_2.tail(List(1,2,3,4,2)) should equal(List(2,3,4,2))
  }

  test("testSetHead") {
    Exercise3_2.setHead(2, Nil) should equal(Nil)
    Exercise3_2.setHead(2, List(1,2,3,4,5)) should equal(List(2, 2,3,4,5))
  }
}
