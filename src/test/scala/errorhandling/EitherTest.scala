package errorhandling

import org.scalatest._

class EitherTest extends FunSuite with Matchers {
  test("testMap2") {
    val mean1 = Either.mean(Vector(1,2,3,4,5))
    val mean2 = Either.mean(Vector(1,2,3,4))
    mean1.map2(mean2)(_ + _) should equal(Right(5.5))
  }

  test("testOrElse") {
    Either.mean(Vector(1,2,3,4,5)) orElse (Left("This is left!")) should equal(Right(3.0))
    Either.mean(Vector(1,2,3,4)) orElse (Left("This is left!")) should equal(Right(2.5))
    Either.mean(Vector()) orElse (Left("This is left!")) should equal(Left("This is left!"))
  }

  test("testFlatMap") {
    Either.mean(Vector(1,2,3,4,5)) flatMap (x => Right(x.toString())) should equal(Right("3.0"))
    Either.mean(Vector(1,2,3,4,5)) flatMap (_ => Left("This is left!")) should equal(Left("This is left!"))
    Either.mean(Vector()) flatMap (x => Right(x.toString())) should equal(Left("mean of empty list!"))
  }

  test("testMap") {
    Either.mean(Vector(1,2,3,4,5)) map (_.toString()) should equal(Right("3.0"))
    Either.mean(Vector()) map (_.toString()) should equal(Left("mean of empty list!"))
  }

  test("testTry") {
    Either.Try(4/2) should equal(Right(2))
    Either.Try(4/0) match {
      case Left(e) => e.getMessage() should equal("/ by zero")
    }
  }

  test("testSafeDiv") {
    Either.safeDiv(4, 2) should equal(Right(2))
    Either.safeDiv(4, 0) match {
      case Left(e) => e.getMessage() should equal("/ by zero")
    }
  }

  test("testMean") {
    Either.mean(Vector(1,2,3,4,5)) should equal(Right(3.0))
    Either.mean(Vector()) should equal(Left("mean of empty list!"))
  }
}
