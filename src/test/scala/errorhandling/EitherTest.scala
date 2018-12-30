package errorhandling

import org.scalatest._

class EitherTest extends FunSuite with Matchers {
  test("testTraverse") {
    val func:Double => Either[String, Double] = (i) => if(i == 0) Left("Zero!") else Right(3.0/i)
    Either.traverse(List(1.0,2.0,3.0,4.0))(func) should equal(Right(List(3.0,1.5,1.0,(3.0/4.0))))
    Either.traverse(List(1.0,0,3.0,4.0))(func) should equal(Left("Zero!"))
  }

  test("testSequence") {
    def go(to:Int, from:Int, xs:List[Either[String, Int]]):List[Either[String, Int]] =
      if(to == from) Right(to) :: xs
      else go(to, from - 1, Right(from) :: xs)

    val list = go(1, 5, Nil)
    Either.sequence(list) should equal(Right(List(1,2,3,4,5)))
    Either.sequence(List(Right(1), Left("This is first left!"), Right(3), Left("This is second left!"))) should equal(Left("This is first left!"))
  }

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
