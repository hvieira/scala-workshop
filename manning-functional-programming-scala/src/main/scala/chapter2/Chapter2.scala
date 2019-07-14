package chapter2

object Chapter2 {

  def fib(n: Int): Int = {

    def inner(desired: Int, curr: Int, prev1: Int, prev2: Int): Int = desired match {
      case x if x == curr => prev1 + prev2
      case _ => inner(desired, curr + 1, prev2, prev1 + prev2)
    }

    n match {
      case 0 => 0
      case 1 => 1
      case _ => inner(n, curr = 2, prev1 = 0, prev2 = 1)
    }
  }

}
