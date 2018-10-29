package fpinscala.errorhandling

import fpinscala.errorhandling.Either._
import org.scalatest._

class EitherSpec extends FunSuite {

  test("traverse") {
    assert(traverse(List(2,3,4))(Right(_)) === Right(List(2,3,4)))
  }

  test("sequence") {
    assert(sequence(List(Right(1), Right(3), Right(7))) === Right(List(1,3,7)))
  }




}
