package errorhandling

import org.scalatest._

class OptionTest extends FunSuite with Matchers {
  test("testMean") {
    Option.mean(List(1.0,2.0,3.0,4.0)) should equal(2.5)
    the [ArithmeticException] thrownBy {
      Option.mean(List())
    } should have message "mean of empty list!"
  }

  test("testFailingFn2") {
    Option.failingFn2(1) should equal(43)
  }

  test("testFailingFn") {
    the [Exception] thrownBy {
      Option.failingFn(1)
    } should have message "fail!"
  }
}
