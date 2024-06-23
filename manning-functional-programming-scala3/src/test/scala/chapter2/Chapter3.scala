import chapter3.{MyList, Nil, Cons}

class Chapter3ListTest extends munit.FunSuite {
  test("tail returns the tail of a list") {
    assertEquals(MyList.tail(Nil), Nil)
    assertEquals(MyList.tail(MyList(1)), Nil)
    assertEquals(MyList.tail(MyList("hello")), Nil)
    assertEquals(MyList.tail(MyList("hello", "world")), MyList("world"))
    assertEquals(MyList.tail(MyList("hello", "world","and", "friends")), MyList("world","and", "friends"))
  }
}