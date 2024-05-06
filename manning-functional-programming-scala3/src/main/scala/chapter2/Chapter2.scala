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
}
