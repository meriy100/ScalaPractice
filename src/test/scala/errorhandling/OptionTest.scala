package errorhandling

import org.scalatest._

class OptionTest extends FunSuite with Matchers {
  test("testFilter") {
    Some(12) filter (i => i > 10) should equal(Some(12))
    Some(10).filter((i:Int) => i > 10) should equal(None)
    None.filter((i:Int) => i > 10) should equal(None)
  }

  test("testOrElse") {
    Some(12) orElse Some(1211) should equal(Some(12))
    None orElse Some(1211) should equal(Some(1211))
  }

  test("testFlatMap") {
    Some(12) flatMap (v => Some(v.toString())) should equal(Some("12"))
    None flatMap (v => Some(v.toString())) should equal(None)
  }

  test("testGetOrElse") {
    Some(12) getOrElse 0 should equal(12)
    None getOrElse (0) should equal(0)
  }

  test("testMap") {
    Some(12) map (v => v.toString()) should equal(Some("12"))
    None map (v => v.toString()) should equal(None)
  }

  test("testMean3") {
    Option.mean3(List(1.0,2.0,3.0,4.0)) should equal(Some(2.5))
    Option.mean3(List()) should equal(None)
  }
  test("testMean2") {
    Option.mean2(List(1.0,2.0,3.0,4.0), 0.0/0.0) should equal(2.5)
    Option.mean2(List(), 0.0) should equal(0.0)
  }
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
