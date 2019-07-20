package chapter2

object Chapter2 {

  def fib(n: Int): Int = {

    @annotation.tailrec
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

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    def loop(i: Int): Boolean = {
      if (as.length - i >= 2)
        if (ordered(as(i), as(i + 1))) loop(i + 1) else false
      else
        true
    }

    if (as.isEmpty) true else loop(0)
  }

  def curry[A, B, C](f: (A, B) => C): A => B => C =
    a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

}
