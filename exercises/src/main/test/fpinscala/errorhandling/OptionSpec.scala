package fpinscala.errorhandling

import fpinscala.errorhandling.Option._
import org.scalatest._

class OptionSpec extends FunSuite {
  test("map") {
    assert(Some(4).map(_ + 3) === Some(7))
    assert(None.map((a: Int) => a + 2) === None)
  }

  test("flatMap") {
    assert(Some(4).flatMap(a => Some(a + 3)) === Some(7))
    assert(None.flatMap((a: Int) => Some(a + 2)) === None)
  }

  test("getOrElse") {
    assert(Some(4).getOrElse(3) === 4)
    assert(None.getOrElse(3) === 3)
  }

  test("orElse") {
    assert(Some(4).orElse(Some(3)) === Some(4))
    assert(None.orElse(Some(3)) === Some(3))
  }

  test("filter") {
    assert(Some(4).filter(_ == 4) === Some(4))
    assert(Some(4).filter(_ < 4) === None)
    assert(None.filter(_ == 1) === None)
  }

  test("map2") {
    assert(map2(Some(4), Some(3))(_ + _) === Some(7))
    assert(map2(None: Option[Int], Some(3))(_ + _) === None)
    assert(map2(Some(3), None: Option[Int])(_ + _) === None)
    assert(map2(None: Option[Int], None: Option[Int])(_ + _) === None)
  }

  test("sequence") {
    assert(sequence(List(Some(3), Some(4))) === Some(List(3,4)))
    assert(sequence(List(None, Some(3))) === None)
  }

}
