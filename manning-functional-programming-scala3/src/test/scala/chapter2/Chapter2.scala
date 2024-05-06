import chapter2.Chapter2

class Chapter2Test extends munit.FunSuite {
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
