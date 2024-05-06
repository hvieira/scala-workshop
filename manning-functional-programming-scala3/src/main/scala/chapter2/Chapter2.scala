package chapter2

object Chapter2 {
  def fib(n: Int): Int = {
    def loop(fib1: Int, fib2: Int, current_fib: Int, wanted_fib: Int): Int = {
      if (wanted_fib == current_fib)
        fib2
      else
        loop(fib2, fib1 + fib2, current_fib + 1, wanted_fib)
    }

    if (n == 1)
      0
    else if (n == 2)
      1
    else
      loop(0, 1, 1, n)
  }

  def findFirstIn[A](collection: Array[A], id: A => Boolean): Int = {
    def loop(n: Int): Int = {
      if (n >= collection.length) -1
      else if (id(collection(n))) n
      else loop(n + 1)
    }
    loop(0)
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    def loop(as: Array[A], i: Int, j: Int): Boolean = {
        if (j >= as.length)
            true
        else if (ordered(as(i), as(j)))
            loop(as, i + 1, j + 1)
        else
            false
    }

    if (as.length <= 1)
        true
    else
        loop(as, 0, 1)
  }
}
