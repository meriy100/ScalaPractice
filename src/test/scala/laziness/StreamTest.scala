package laziness

import org.scalatest._

class StreamTest extends FunSuite with Matchers {

  test("testToList") {
    Stream.cons(1, Stream.cons(2, Stream.empty)).toList should equal(List(1, 2))
  }

  test("testHeadOption") {
    Stream.cons(1, Stream.cons(2, Stream.empty)).headOption should equal(Some(1))
  }
}
