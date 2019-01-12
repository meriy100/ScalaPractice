package laziness

import org.scalatest._
import Stream._

class StreamTest extends FunSuite with Matchers {
  test("testOnes2") {
    Stream.ones2.take(4).toList should equal(List(1, 1, 1, 1))
  }

  test("testConstant2") {
    Stream.constant2(4).map(_ + 3).take(4).toList should equal(List(7, 7, 7, 7))
  }

  test("testFrom2") {
    Stream.from2(4).map(_ + 4).take(5).toList should equal(List(8, 9, 10, 11, 12))
  }

  test("testFibs2") {
    Stream.fibs2.take(7).toList should equal(List(0, 1, 1, 2, 3, 5, 8))
  }

  test("testUnfold") {
    Stream.unfold(1)((x) => Some((x.toString, x + 2))).take(3).toList should equal(List("1", "3", "5"))
    Stream.unfold(1)((x) => if(x % 5 == 0) None else Some((x.toString, x + 2))).toList should equal(List("1", "3"))

  }

  test("testFibs") {
    Stream.fibs.take(7).toList should equal(List(0, 1, 1, 2, 3, 5, 8))
  }

  test("testFrom") {
    Stream.from(4).map(_ + 4).take(5).toList should equal(List(8, 9, 10, 11, 12))
  }

  test("testConstant") {
    Stream.constant(4).map(_ + 3).take(4).toList should equal(List(7, 7, 7, 7))
  }

  test("testFind") {
    Stream(1,2,3,4).map(_ + 10).find(_ % 2 == 0) should equal(Some(12))
  }

  test("testFlatMap") {
    Stream(1,2,3,4,5).flatMap(x => Stream(x + 1)).toList should equal(List(2, 3, 4, 5, 6))
  }

  test("testAppend") {
    Stream(1,2,3,4,5).append(Stream(6,7,8)).toList should equal(List(1, 2, 3, 4, 5, 6, 7, 8))
    Stream(1,2,3,4,5).append(cons(1, cons({ print("Never Called"); 2 }, Stream.empty)))
  }

  test("testFilter") {
    Stream(1,2,3,4,5).filter(_ % 2 == 1).toList should equal(List(1, 3, 5))
  }

  test("testMap") {
    Stream(1,2,3,4,5).map(_+ 1).toList should equal(List(2, 3, 4, 5, 6))
  }

  test("testHeadOption2") {
    cons(1, cons({ print("Never Called"); 2 }, Stream.empty)).headOption2 should equal(Some(1))
  }

  test("testTakeWhile2") {
    Stream(1, 2, 3, 4, 5).takeWhile2(_ < 3).toList should equal(List(1, 2))
  }

  test("testForAll") {
    Stream(1, 2, 3).forAll(_ < 5) should be(true)
    Stream(1, 2, 3).forAll(_ < 3) should be(false)
    Cons(()=> 1, () => Cons(() => {print("Never Called"); 2}, () => Empty)).forAll(_ < 1) should be(false)
    cons(1, cons({ print("Never Called"); 2 }, Stream.empty)).forAll(_ < 1) should be(false)
  }

  test("testExists") {
    Stream(1, 2, 3).exists(_ == 3) should be(true)
    Stream(1, 2, 3).exists(_ == 4) should be(false)
    Stream(1, 2, 3).exists(_ == 1) should be(true)
    Cons(()=> 1, () => Cons(() => {print("Never Called"); 2}, () => Empty)).exists(_ == 1) should be(true)
    cons(1, cons({ print("Never Called"); 2 }, Stream.empty)).exists(_ == 1) should be(true)
  }

  test("testTakeWhile") {
    Stream(1, 2, 3, 4, 5).takeWhile(_ < 3).toList should equal(List(1, 2))
  }

  test("testDrop") {
    Stream(1, 2, 3, 4, 5).drop(3).toList should equal(List(4, 5))
  }

  test("testToTake") {
    cons(1, cons(2, Stream.empty)).take(1).toList should equal(List(1))
  }

  test("testToList") {
    cons(1, cons(2, Stream.empty)).toList should equal(List(1, 2))
  }

  test("testHeadOption") {
    cons(1, Stream.cons(2, Stream.empty)).headOption should equal(Some(1))
  }
}
