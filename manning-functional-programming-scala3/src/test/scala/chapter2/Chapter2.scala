import chapter2.Chapter2

class Chapter2FibonaciTest extends munit.FunSuite {
  test("The first fib is 0") {
    assertEquals(0, Chapter2.fib(1))
  }
  test("The second fib is 1") {
    assertEquals(1, Chapter2.fib(2))
  }
  test("The third fib is 2") {
    assertEquals(2, Chapter2.fib(3))
  }
  test("The fourth fib is 3") {
    assertEquals(3, Chapter2.fib(4))
  }
  test("The fifth fib is 5") {
    assertEquals(5, Chapter2.fib(5))
  }
  test("The sixth fib is 8") {
    assertEquals(8, Chapter2.fib(6))
  }
}

class Chapter2HOFTest extends munit.FunSuite {
  val ascendingIntegerSortFun = (n1: Int, n2: Int) => n1 <= n2
  test("sorted considers empty and single item arrays as sorted") {
    assertEquals(true, Chapter2.isSorted(Array[Int](), ascendingIntegerSortFun))
    assertEquals(
      true,
      Chapter2.isSorted(Array[Int](1), ascendingIntegerSortFun)
    )
    assertEquals(
      true,
      Chapter2.isSorted(Array[Int](7), ascendingIntegerSortFun)
    )
  }

  test(
    "sorted considers arrays with elements in order according to the provided function as sorted"
  ) {
    assertEquals(
      true,
      Chapter2.isSorted(Array[Int](1, 1), ascendingIntegerSortFun)
    )
    assertEquals(
      true,
      Chapter2.isSorted(Array[Int](1, 2), ascendingIntegerSortFun)
    )
    assertEquals(
      true,
      Chapter2.isSorted(Array[Int](7, 7, 8), ascendingIntegerSortFun)
    )
  }

  test(
    "sorted considers arrays with elements NOT in order according to the provided function as not sorted"
  ) {
    assertEquals(
      false,
      Chapter2.isSorted(Array[Int](1, 0), ascendingIntegerSortFun)
    )
    assertEquals(
      false,
      Chapter2.isSorted(Array[Int](2, 1), ascendingIntegerSortFun)
    )
    assertEquals(
      false,
      Chapter2.isSorted(Array[Int](3, 7, 4), ascendingIntegerSortFun)
    )
  }
}
