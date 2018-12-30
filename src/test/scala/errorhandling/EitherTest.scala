package errorhandling

import org.scalatest._

class EitherTest extends FunSuite with Matchers {
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
