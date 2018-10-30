package fpinscala.lazyness

import fpinscala.laziness._
import org.scalatest._

class StreamSpec extends FunSuite {

  test("toList") {
    assert(Stream(1,2,3,4).toList === List(1,2,3,4))
  }

  test("take") {
    assert(Stream(1,2,3,4).take(2).toList === List(1,2))
  }

  test("drop") {
    assert(Stream(1,2,3,4).drop(2).toList === List(3,4))
  }

  test("takeWhile") {
    assert(Stream(1,2,3,4).takeWhile(_ < 3).toList === List(1, 2))
  }

  test("forall true") {
    assert(Stream(1,2,3,4).forAll(_ < 99) === true)
  }

  test("forall false") {
    assert(Stream(1,2,3,4).forAll(_ < 0) === false)
  }

  test("takeWhile2") {
    assert(Stream(1,2,3,4,1,1).takeWhile2(_ < 3).toList === List(1,2))
  }

  test("headOption present") {
    assert(Stream(1,2,3,4,1,1).headOption === Some(1))
  }

  test("headOption missing") {
    assert(Empty.headOption === None)
  }

  test("map") {
    assert(Stream(1,2,3,4).map(_ * 2).toList === List(2,4,6,8))
  }

  test("filter") {
    assert(Stream(1,2,3,4,1,1).filter(_ < 3).toList === List(1,2,1,1))
  }

  test("constant") {
    assert(Stream.constant(1).take(5).toList === List(1,1,1,1,1))
  }

  test("from") {
    assert(Stream.from(1).take(5).toList === List(1,2,3,4,5))
  }

  test("fibs") {
    assert(Stream.fibs.take(11).toList === List(0,1,1,2,3,5,8,13,21,34,55))
  }

  test("constant1") {
    assert(Stream.constant1(1).take(5).toList === List(1,1,1,1,1))
  }

  test("from1") {
    assert(Stream.from1(1).take(5).toList === List(1,2,3,4,5))
  }

  test("fibs1") {
    assert(Stream.fibs1.take(11).toList === List(0,1,1,2,3,5,8,13,21,34,55))
  }

  test("map1") {
    assert(Stream(1,2,3,4).map1(_ * 2).toList === List(2,4,6,8))
  }

  test("take1") {
    assert(Stream(1,2,3,4).take1(2).toList === List(1,2))
    assert(Stream(1,2,3,4).take1(1).toList === List(1))
    assert(Stream(1,2,3,4).take1(0).toList === List())
  }

  test("takeWhile1") {
    assert(Stream(1,2,3,4,1,1).takeWhile1(_ < 3).toList === List(1,2))
  }

  test("zip with") {
    assert(Stream(1,2,3).zipWith(Stream(2,3))(_ + _).toList === List(3,5))
  }

  test("starts with") {
    assert(Stream(1,2,3).startsWith(Stream(1,2)) === true)
    assert(Stream(1,2,3).startsWith(Stream(9,1,2)) === false)
  }

  test("tails") {
    assert(Stream(1,2,3).tails.map(_.toList).toList === List(List(1,2,3), List(2,3), List(3), List()))
  }
}
