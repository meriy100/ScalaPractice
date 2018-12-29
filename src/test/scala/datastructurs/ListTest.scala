package datastructurs

import org.scalatest._

class ListTest extends FunSuite with Matchers {
  def go(to:Int, from:Int, xs:List[Int]):List[Int] =
    if(to == from) Cons(to, xs)
    else go(to, from - 1, Cons(from, xs))

  test("testSelect") {
    val xs = go(1,10,Nil:List[Int])
    List.select(xs)(_ % 2 == 0) should equal(List(2,4,6,8,10))
  }
  test("testFilter") {
    val xs = go(1,10,Nil:List[Int])
    List.filter(xs)(_ == 3) should equal(go(3,10,Nil:List[Int]))
  }

  test("testMap") {
    List.map(List.join(List(List(1.0,2.1,3.2), List(4.3,5.4,6.5))))((x:Double) => x.toString()) should
      equal(List("1.0","2.1","3.2","4.3","5.4","6.5"))
    List.map(List.join(List(List(1,2,3), List(4,5,6), List(7,8,9))))(x => x + 1) should equal(List(2,3,4,5,6,7,8,9,10))
  }

  test("testEachToString") {
    List.eachToString(List.join(List(List(1.0,2.1,3.2), List(4.3,5.4,6.5)))) should
      equal(List("1.0","2.1","3.2","4.3","5.4","6.5"))
  }
  test("testEachInc") {
    List.eachInc(List.join(List(List(1,2,3), List(4,5,6), List(7,8,9)))) should equal(List(2,3,4,5,6,7,8,9,10))
    val xs = List(2,3,1,2)
    List.eachInc(xs) should equal(List(3,4,2,3))
    xs should equal(List(2,3,1,2))
  }
  test("testJoin") {
    List.join(List(List(1,2,3), List(4,5,6), List(7,8,9))) should equal(List(1,2,3,4,5,6,7,8,9))
  }

  test("testAppend2") {
    List.append2(List(1,2,3), List(3,4,5)) should equal(List(1,2,3,3,4,5))
    val xs = List(12,3,4,2,1,3)
    val ys = List(13,4,5,3,2,4)
    List.append2(xs, ys) should equal(List.append(xs, ys))
  }

  test("testAppend") {
    List.append(List(1,2,3), List(3,4,5)) should equal(List(1,2,3,3,4,5))
  }

  test("testExercise3.8") {
    List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_, _)) should equal(List(1,2,3))
  }

  test("testReverse") {
    List.reverse(Nil) should equal(Nil)
    List.reverse(List(1, 2)) should equal (List.reverse(Cons(1, Cons(2, Nil))))
    List.reverse(Cons(100, List(1, 2))) should equal(List(2, 1, 100))
    List.reverse(List(2,1,2,3)) should equal(List(3, 2, 1, 2))
    List.foldRight(List(2,1,3), (b:List[Int]) => b)((a,g) => b => g(Cons(a, b)))(Nil:List[Int])
  }

  test("testLength") {
    List.length(Nil) should equal(0)
    List.length(List(1, 2)) should equal (List.length(Cons(1, Cons(2, Nil))))
    List.length(Cons(100, List(1, 2))) should equal(1 + List.length(List(1, 2)))
    List.length(List(2,1,2,3)) should equal(4)
    List.length(List(2,1,2,3)) + List.length(Cons(1, List(1, 2))) should equal(List.length(List(2,1,2,3, 1, 1, 2)))
  }

  test("testLength2") {
    List.length2(Nil) should equal(0)
    List.length2(List(1, 2)) should equal (List.length(Cons(1, Cons(2, Nil))))
    List.length2(Cons(2, List(1, 2))) should equal(1 + List.length(List(1, 2)))
    List.length2(List(2,1,2,3)) should equal(4)
    List.length2(List(2,1,2,3)) + List.length2(Cons(1, List(1, 2))) should equal(List.length(List(2,1,2,3, 1, 1, 2)))
  }

  test("testSum3") {
    List.sum3(Nil) should equal(0)
    List.sum3(List(1, 2)) should equal (List.sum2(Cons(1, Cons(2, Nil))))
    List.sum3(Cons(1, List(1, 2))) should equal(1 + List.sum2(List(1, 2)))
    List.sum3(List(1, 2)) should equal (3)
    List.sum3(List(1,2,3,4,5)) should equal(15)
  }

  test("testProduct3") {
    List.product3(Nil) should equal(1)
    List.product3(List(0)) should equal(0)
    List.product3(List(1, 2, 0)) should equal(0)
    List.product3(List(1, 2, 3, 4, 5)) should equal(120)
    List.product3(Cons(1, List(1, 2))) should equal(1 * List.product2(List(1, 2)))
  }

  test("testSum") {
    List.sum(Nil) should equal(0)
    List.sum(List(1, 2)) should equal (List.sum(Cons(1, Cons(2, Nil))))
    List.sum(Cons(1, List(1, 2))) should equal(1 + List.sum(List(1, 2)))
  }

  test("testSum2") {
    List.sum2(Nil) should equal(0)
    List.sum2(List(1, 2)) should equal (List.sum2(Cons(1, Cons(2, Nil))))
    List.sum2(Cons(1, List(1, 2))) should equal(1 + List.sum2(List(1, 2)))
  }

  test("testProduct") {
    List.product(Nil) should equal(1)
    List.product(List(0)) should equal(0)
    List.product(List(1, 2, 0)) should equal(0)
    List.product(List(1, 2, 3, 4, 5)) should equal(120)
    List.product(Cons(1, List(1, 2))) should equal(1 * List.product(List(1, 2)))
  }

  test("testProduct2") {
    List.product2(Nil) should equal(1)
    List.product2(List(0)) should equal(0)
    List.product2(List(1, 2, 0)) should equal(0)
    List.product2(List(1, 2, 3, 4, 5)) should equal(120)
    List.product2(Cons(1, List(1, 2))) should equal(1 * List.product2(List(1, 2)))
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

