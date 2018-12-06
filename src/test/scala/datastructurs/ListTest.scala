package datastructurs

import org.scalatest._

class ListTest extends FunSuite with Matchers {
  test("testList") {
    List.sum(List(1, 2)) should equal (List.sum(Cons(1, Cons(2, Nil))))
  }

  test("testSum") {
    List.sum(Nil) should equal(0)
    List.sum(Cons(1, List(1, 2))) should equal(1 + List.sum(List(1, 2)))
  }
}
