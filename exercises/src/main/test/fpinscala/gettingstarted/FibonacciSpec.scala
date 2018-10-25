package fpinscala.gettingstarted

import fpinscala.gettingstarted.MyModule.fib
import org.scalatest._

class FibonacciSpec extends FunSuite {
  test("0 should be 0") {
    assert(fib(0) == 0)
  }

  test("1 should be 1") {
    assert(fib(1) == 1)
  }

  test("2 should be 1") {
    assert(fib(2) == 1)
  }

  test("8 should be 34") {
    assert(fib(9) == 34)
  }

  test("10 should be 8 + 9") {
    assert(fib(10) == fib(8) + fib(9))
  }
}
