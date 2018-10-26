package fpinscala.datastructures

import fpinscala.datastructures.List._
import org.scalatest._

class ListDataStructuresSpec extends FunSuite {
  test("x pattern matching should be 3") {
    assert(List.x == 3)
  }

  test("tail of list") {
    assert(tail(List(1,2,3,4)) == List(2,3,4))
  }

  test("sethead of list") {
    assert(setHead(List(1,2,3,4), 99) == List(99,2,3,4))
  }

  test("drop from list") {
    assert(drop(List(1,2,3,4,5), 2) == List(3,4,5))
  }

  test("drop while from list") {
    assert(dropWhile[Int](List(1, 2, 3, 4, 5), _ < 4) == List(4,5))
  }

  test("init") {
    assert(init(List(1, 2, 3, 4, 5)) == List(1,2,3,4))
  }

  test("length") {
    assert(length(List(1, 2, 3, 4, 5)) == 5)
  }

  test("length3") {
    assert(length3(List(1, 2, 3, 4, 5)) == 5)
  }

  test("sum3") {
    assert(sum3(List(1, 2, 3, 4, 5)) == 15)
  }

  test("product3") {
    assert(product3(List(1, 2, 3, 4, 5)) == 120)
  }

  test("reverse") {
    assert(reverse(List(1, 2, 3, 4, 5)) == List(5,4,3,2,1))
  }

  test("appendFold") {
    assert(appendFold(List(1, 2, 3),List(4, 5)) == List(1,2,3,4,5))
  }

  test("concat") {
    assert(concat(List(1, 2, 3),List(4, 5), List(6), Nil, List(7)) == List(1,2,3,4,5,6,7))
  }

  test("add one") {
    assert(addOne(List(1, 2, 3)) == List(2,3,4))
  }

  test("double to string") {
    assert(doubleToString(List(0.1, 0.33)) == List("0.1", "0.33"))
  }

  test("map") {
    assert(map(List(1, 33, 9))(_ * 10) == List(10, 330, 90))
  }

  test("filter") {
    assert(filter(List(1, 2, 4, 33, 9))(_ % 2 == 0) == List(2,4))
  }

  test("flatmap") {
    assert(flatMap(List(1, 2, 4))(el => List(el, el, el)) == List(1,1,1,2,2,2,4,4,4))
  }

  test("zip with") {
    assert(zipWith(List(1, 2, 4), List(33, 9))(_ + _) == List(34, 11))
  }

  test("has subsequence") {
    assert(hasSubsequence(List(1,2,3,4), List(2,3)))
    assert(hasSubsequence(List(1,2,3,4), List(1,2,3,4)))
    assert(hasSubsequence(List(1,2,3,4), List(1,4)))
  }

}
