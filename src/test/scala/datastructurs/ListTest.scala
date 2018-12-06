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

  test("testTail") {
    List.tail(Nil) should equal(Nil)
    List.tail(List(1,2,3,4,5)) should equal(List(2,3,4,5))
    List.tail(List(1,2,3,4,2)) should equal(List(2,3,4,2))
  }

  test("testSetHead") {
    List.setHead(2, Nil) should equal(Nil)
    List.setHead(2, List(1,2,3,4,5)) should equal(List(2, 2,3,4,5))
  }

  test("testDrop") {
    List.drop(Nil, 9) should equal(Nil)
    List.drop(List(1,2,3,4,5,6,7), 3) should equal(List(4,5,6,7))
    List.drop(List(1,2,3,4,5,6,7), 10) should equal(Nil)
  }

  test("testDropWhile") {
    List.dropWhile(Nil, (_:Any) => true) should equal(Nil)
    List.dropWhile(List(1,2,3,4,5,6,7), (n:Int) => (n < 5)) should equal(List(5, 6,7))
    List.dropWhile(List(1,2,3,4,5,6,7), (n:Int) => (n < 10)) should equal(Nil)
    List.dropWhile(List("abc", "abcd", "abcdes", "acdb"), (n:String) => (n.startsWith("ab"))) should equal(List("acdb"))
  }
}

