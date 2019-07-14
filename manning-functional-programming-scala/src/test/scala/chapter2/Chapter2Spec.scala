package chapter2

import org.scalatest.{FlatSpec, Matchers}

class Chapter2Spec extends FlatSpec with Matchers {

  "Fib of 0" should "be 0" in {
    Chapter2.fib(0) shouldEqual 0
  }

  "Fib of 1" should "be 1" in {
    Chapter2.fib(1) shouldEqual 1
  }

  "Fib of 2" should "be 1" in {
    Chapter2.fib(2) shouldEqual 1
  }

  "Fib of 3" should "be 2" in {
    Chapter2.fib(3) shouldEqual 2
  }

  "Fib of 4" should "be 3" in {
    Chapter2.fib(4) shouldEqual 3
  }

  "Fib of 5" should "be 5" in {
    Chapter2.fib(5) shouldEqual 5
  }

  "Fib of 6" should "be 8" in {
    Chapter2.fib(6) shouldEqual 8
  }

  "Fib of 7" should "be 13" in {
    Chapter2.fib(7) shouldEqual 13
  }

  "Fib of 13" should "be 233" in {
    Chapter2.fib(13) shouldEqual 233
  }

}
