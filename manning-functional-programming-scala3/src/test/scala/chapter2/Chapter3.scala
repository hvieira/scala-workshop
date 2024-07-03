import chapter3.{MyList, Nil, Cons}

class Chapter3ListTest extends munit.FunSuite {
  test("tail returns the tail of a list") {
    assertEquals(MyList.tail(Nil), Nil)
    assertEquals(MyList.tail(MyList(1)), Nil)

    assertEquals(MyList.tail(MyList("hello")), Nil)
    assertEquals(MyList.tail(MyList("hello", "world")), MyList("world"))
    assertEquals(MyList.tail(MyList("hello", "world","and", "friends")), MyList("world","and", "friends"))
  }

  test("setHead prepends the new element to the list") {
    assertEquals(MyList.setHead(1, Nil), MyList(1))
    assertEquals(MyList.setHead(1, MyList(0)), MyList(1, 0))
    assertEquals(MyList.setHead(1, MyList(0, -1)), MyList(1, 0, -1))
    
    assertEquals(MyList.setHead("John", MyList("Doe")), MyList("John", "Doe"))
  }

  test("drop from an empty list returns an empty list") {
    assertEquals(MyList.drop(Nil, 0), Nil)
    assertEquals(MyList.drop(Nil, 1), Nil)
    assertEquals(MyList.drop(Nil, 2), Nil)
    assertEquals(MyList.drop(Nil, 3), Nil)
    assertEquals(MyList.drop(Nil, 100000), Nil)
  }

  test("drop from a list drops the first N items") {
    assertEquals(MyList.drop(MyList(1,2,3,4,5), 0), MyList(1,2,3,4,5))
    assertEquals(MyList.drop(MyList(1,2,3,4,5), 1), MyList(2,3,4,5))
    assertEquals(MyList.drop(MyList(1,2,3,4,5), 2), MyList(3,4,5))
    assertEquals(MyList.drop(MyList(1,2,3,4,5), 3), MyList(4,5))
    assertEquals(MyList.drop(MyList(1,2,3,4,5), 4), MyList(5))
    assertEquals(MyList.drop(MyList(1,2,3,4,5), 5), Nil)
  }

  test("dropWhile from an empty list returns an empty list") {
    assertEquals(MyList.dropWhile(Nil, _ => true ), Nil)
  }

  test("dropWhile from a list returns that list without the first elements that fulfil the predicate") {
    assertEquals(MyList.dropWhile(MyList(1,2,3,4,5,6), x => x < 5 ), MyList(5, 6))
    assertEquals(MyList.dropWhile(MyList(1,2,3,4,5,6), x => x <= 5 ), MyList(6))
    assertEquals(MyList.dropWhile(MyList(1,2,3,4,5,6), x => x < 0 ), MyList(1,2,3,4,5,6))
  }

  test("init returns the list without the last element") {
    assertEquals(MyList.init(Nil), Nil)
    assertEquals(MyList.init(MyList(1)), Nil)
    assertEquals(MyList.init(MyList(1,2)), MyList(1))
    assertEquals(MyList.init(MyList(1,2,3)), MyList(1,2))
    
    assertEquals(MyList.init(MyList("hello","world")), MyList("hello"))
  }
}