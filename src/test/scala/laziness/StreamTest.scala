package laziness

import org.scalatest._

class StreamTest extends FunSuite with Matchers {
  test("testTakeWhile") {
    Stream(1, {print("call me once"); 2}, 3, 4, 5).takeWhile(_ < 3).toList should equal(List(1, 2))
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
