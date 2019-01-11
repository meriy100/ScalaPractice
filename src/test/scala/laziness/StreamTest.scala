package laziness

import org.scalatest._

class StreamTest extends FunSuite with Matchers {
  test("testTakeWhile2") {
    Stream(1, 2, 3, 4, 5).takeWhile2(_ < 3).toList should equal(List(1, 2))
  }

  test("testForAll") {
    Stream(1, 2, 3).forAll(_ < 5) should be(true)
    Stream(1, 2, 3).forAll(_ < 3) should be(false)
    Cons(()=> 1, () => Cons(() => {print("Never Called"); 2}, () => Empty)).forAll(_ < 1) should be(false)
    Stream.cons(1, Stream.cons({ print("Never Called"); 2 }, Stream.empty)).forAll(_ < 1) should be(false)
  }

  test("testExists") {
    Stream(1, 2, 3).exists(_ == 3) should be(true)
    Stream(1, 2, 3).exists(_ == 4) should be(false)
    Stream(1, 2, 3).exists(_ == 1) should be(true)
    Cons(()=> 1, () => Cons(() => {print("Never Called"); 2}, () => Empty)).exists(_ == 1) should be(true)
    Stream.cons(1, Stream.cons({ print("Never Called"); 2 }, Stream.empty)).exists(_ == 1) should be(true)
  }

  test("testTakeWhile") {
    Stream(1, 2, 3, 4, 5).takeWhile(_ < 3).toList should equal(List(1, 2))
  }

  test("testDrop") {
    Stream(1, 2, 3, 4, 5).drop(3).toList should equal(List(4, 5))
  }

  test("testToTake") {
    Stream.cons(1, Stream.cons(2, Stream.empty)).take(1).toList should equal(List(1))
  }

  test("testToList") {
    Stream.cons(1, Stream.cons(2, Stream.empty)).toList should equal(List(1, 2))
  }

  test("testHeadOption") {
    Stream.cons(1, Stream.cons(2, Stream.empty)).headOption should equal(Some(1))
  }
}
