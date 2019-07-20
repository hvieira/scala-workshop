package chapter2

import org.scalatest.{Matchers, WordSpec}

class Chapter2Spec extends WordSpec with Matchers {

  "fib" must {
    "be 0 for n 0" in {
      Chapter2.fib(0) shouldEqual 0
    }

    "be 1 for n 1" in {
      Chapter2.fib(1) shouldEqual 1
    }

    "be 1 for n 2" in {
      Chapter2.fib(2) shouldEqual 1
    }

    "be 2 for n 3" in {
      Chapter2.fib(3) shouldEqual 2
    }

    "be 3 for n 4" in {
      Chapter2.fib(4) shouldEqual 3
    }

    "be 5 for n 5" in {
      Chapter2.fib(5) shouldEqual 5
    }

    "be 8 for n 6" in {
      Chapter2.fib(6) shouldEqual 8
    }

    "be 13 for n 7" in {
      Chapter2.fib(7) shouldEqual 13
    }

    "be 233 for n 13" in {
      Chapter2.fib(13) shouldEqual 233
    }

  }

  "isSorted" must {

    val intAscOrderPredicate: (Int, Int) => Boolean = (a: Int, b: Int) => a < b
    val intDescOrderPredicate: (Int, Int) => Boolean = (a: Int, b: Int) => a > b

    "say an array is sorted if empty" in {
      Chapter2.isSorted(Array.empty[Int], intAscOrderPredicate) should be(true)
      Chapter2.isSorted(Array.empty[Int], intDescOrderPredicate) should be(true)
    }

    "say an array is sorted if has a single value" in {
      Chapter2.isSorted(Array(1), intAscOrderPredicate) should be(true)
      Chapter2.isSorted(Array(456812465), intAscOrderPredicate) should be(true)

      Chapter2.isSorted(Array(1), intDescOrderPredicate) should be(true)
      Chapter2.isSorted(Array(456812465), intDescOrderPredicate) should be(true)
    }

    "say an array is sorted if sorted as expected by the provided function" in {
      Chapter2.isSorted(Array(1, 2), intAscOrderPredicate) should be(true)
      Chapter2.isSorted(Array(1, 2, 3), intAscOrderPredicate) should be(true)
      Chapter2.isSorted(Array(3, 84795), intAscOrderPredicate) should be(true)

      Chapter2.isSorted(Array(2, 1), intDescOrderPredicate) should be(true)
      Chapter2.isSorted(Array(3, 2, 1), intDescOrderPredicate) should be(true)
      Chapter2.isSorted(Array(84795, 3), intDescOrderPredicate) should be(true)
    }

    "say an array is NOT sorted if NOT sorted as expected by the provided function" in {
      Chapter2.isSorted(Array(3, 1), intAscOrderPredicate) should be(false)
      Chapter2.isSorted(Array(3, 2, 1), intAscOrderPredicate) should be(false)
      Chapter2.isSorted(Array(3, 1, 2), intAscOrderPredicate) should be(false)
      Chapter2.isSorted(Array(2, 1, 3), intAscOrderPredicate) should be(false)
      Chapter2.isSorted(Array(2, 3, 1), intAscOrderPredicate) should be(false)
      Chapter2.isSorted(Array(84795, 3), intAscOrderPredicate) should be(false)

      Chapter2.isSorted(Array(1, 3), intDescOrderPredicate) should be(false)
      Chapter2.isSorted(Array(1, 2, 3), intDescOrderPredicate) should be(false)
      Chapter2.isSorted(Array(3, 1, 2), intDescOrderPredicate) should be(false)
      Chapter2.isSorted(Array(2, 1, 3), intDescOrderPredicate) should be(false)
      Chapter2.isSorted(Array(3, 84795), intDescOrderPredicate) should be(false)
    }

  }

  "curry" should {
    "work" in {
      Chapter2.curry((a: Int, b: Int) => a + b)(2)(3) shouldEqual 5
    }
  }

  "uncurry" should {
    "work" in {
      Chapter2.uncurry((a: Int) => (b:Int) => a + b)(2, 3) shouldEqual 5
    }
  }

  "compose" should {
    "work" in {
      Chapter2.compose((b: Int) => b.toString, (a: Int) => a * a)(3) shouldEqual "9"
    }
  }

}
