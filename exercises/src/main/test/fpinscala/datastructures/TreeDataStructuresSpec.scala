package fpinscala.datastructures

import fpinscala.datastructures.Tree._
import org.scalatest._

class TreeDataStructuresSpec extends FunSuite {
  test("size") {
    assert(size(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) === 5)
  }

  test("max") {
    assert(max(Branch(Branch(Leaf(10), Leaf(2)), Leaf(3))) === 10)
  }

  test("depth") {
    assert(depth(Branch(Branch(Branch(Leaf(10), Leaf(2)), Leaf(3)), Leaf(33))) === 3)
  }

  test("map") {
    assert(map(Branch(Branch(Leaf(10), Leaf(2)), Leaf(3)))(_ * 2) === Branch(Branch(Leaf(20), Leaf(4)), Leaf(6)))
  }
}
