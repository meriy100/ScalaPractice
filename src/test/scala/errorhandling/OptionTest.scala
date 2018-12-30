package errorhandling

import org.scalatest._

class OptionTest extends FunSuite with Matchers {
  test("testTraverse") {
    Option.traverse(List(1,2,3))(i => Some(i + 1)) should equal(Some(List(2,3,4)))
  }

  test("testSequence") {
    def go(to:Int, from:Int, xs:List[Option[Int]]):List[Option[Int]] =
      if(to == from) Some(to) :: xs
      else go(to, from - 1, Some(from) :: xs)

    val list = go(1, 5, Nil)
    Option.sequence(list) should equal(Some(List(1,2,3,4,5)))
  }

  test("testMap2") {
    Option.map2(Some(List(1,2,3)), Some(List(4,5,6)))(_ ++ _) should equal(Some(List(1,2,3,4,5,6)))
    Option.map2(None:Option[List[Int]], Some(List(4,5,6)))(_ ++ _) should equal(None)
    Option.map2(Some(List(1,2,3)), None:Option[List[Int]])(_ ++ _) should equal(None)
  }

  test("testList") {
    val abs0: Option[Double] => Option[Double] = Option.lift(math.abs)
    abs0(Some(1.21)) should equal(Some(1.21))
    abs0(Some(-3.21)) should equal(Some(3.21))
    abs0(None) should equal(None)
  }

  test("testVariance") {
    Option.variance(List(1.0,2.0,3.0,4.0,5.0)) should equal(Some(2.0))
    Option.variance(List()) should equal(None)
  }

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
