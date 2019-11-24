package chapter3

import org.scalatest.{Matchers, WordSpec}
import List._

class Chapter3Spec extends WordSpec with Matchers {

  "3.1" must {
    "be" in {
      val res = List(1, 2, 3, 4, 5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case Cons(h, t) => h + sum(t)
        case _ => 101
      }
      res should be(3)
    }
  }

  "3.2 tail" must {
    "nil if empty" in {
      List().tail should be(Nil)
      // --
      List.tail(List()) should be(Nil)
    }
    "nil if single element" in {
      List(1).tail should be(Nil)
      // --
      List.tail(List(1)) should be(Nil)
    }
    "return the tail of 2+ element lists" in {
      List(1, 2).tail should be(List(2))
      List(4, 7, 5).tail should be(List(7, 5))
      // --
      List.tail(List(1, 2)) should be(List(2))
      List.tail(List(4, 7, 5)) should be(List(7, 5))
    }
  }

  "3.3 setHead" must {
    "define a list with a single element with the provided head if its an empty list" in {
      List().setHead(1) should be(List(1))
      // --
      List.setHead(List(), 1) should be(List(1))
    }
    "define a list with the parameter as head if its a non empty list" in {
      List(1).setHead(0) should be(List(0, 1))
      // --
      List.setHead(List(1), 0) should be(List(0, 1))
    }
  }

  "3.4 drop" must {

    "return the same list if 0 or less to drop" in {
      List().drop(0) should be(List())
      List(1).drop(0) should be(List(1))

      List().drop(-1) should be(List())
      List(1).drop(-1) should be(List(1))
      // --
      List.drop(List(), 0) should be(List())
      List.drop(List(1), 0) should be(List(1))

      List.drop(List(), -1) should be(List())
      List.drop(List(1), -1) should be(List(1))
    }

    "return a list with the first n dropped elements" in {
      List().drop(10000) should be(List())
      List(1).drop(1) should be(List())
      List(1, 1).drop(1) should be(List(1))
      List(1, 1, 1).drop(2) should be(List(1))
      List(1, 1, 1).drop(3) should be(List())
      List(1, 1, 1, 1, 1).drop(3) should be(List(1, 1))
      // --
      List.drop(List(), 10000) should be(List())
      List.drop(List(1), 1) should be(List())
      List.drop(List(1, 1), 1) should be(List(1))
      List.drop(List(1, 1, 1), 2) should be(List(1))
      List.drop(List(1, 1, 1), 3) should be(List())
      List.drop(List(1, 1, 1, 1, 1), 3) should be(List(1, 1))
    }
  }

  "3.5 dropWhile" must {

    "return a list with dropped elements while predicate holds" in {
      val predicate = (x: Int) => x < 3

      List().dropWhile(predicate) should be(List())
      List(1).dropWhile(predicate) should be(List())
      List(1, 2).dropWhile(predicate) should be(List())
      List(1, 2, 3).dropWhile(predicate) should be(List(3))
      List(3, 1).dropWhile(predicate) should be(List(3, 1))
      // --
      List.dropWhile(List(), predicate) should be(List())
      List.dropWhile(List(1), predicate) should be(List())
      List.dropWhile(List(1, 2), predicate) should be(List())
      List.dropWhile(List(1, 2, 3), predicate) should be(List(3))
      List.dropWhile(List(3, 1), predicate) should be(List(3, 1))
    }

  }

  "3.6 init" must {

    "return a list with all but the last element" in {
      List.init(List()) should be(List())
      List.init(List(1)) should be(List())
      List.init(List(1, 2)) should be(List(1))
      List.init(List(3, 5, 1)) should be(List(3,5))
    }

  }

  "3.9 length" must {

    "return the length of the list" in {
      List.length(List()) should be(0)
      List.length(List(1)) should be(1)
      List.length(List(1, 1, 7)) should be(3)
      List.length(List(1, 1, 2, 3, 5, 8, 13)) should be(7)
    }

  }

  "3.10 foldLeft" must {

    val lengthFunction = (c: Int, _: Int) => c + 1

    "apply the function from left to right" in {
      List.foldLeft(List(), 0)(lengthFunction) should be(0)
      List.foldLeft(List(1), 0)(lengthFunction) should be(1)
      List.foldLeft(List(1, 1, 7), 0)(lengthFunction) should be(3)
      List.foldLeft(List(1, 1, 2, 3, 5, 8, 13), 0)(lengthFunction) should be(7)

      List.foldLeft(List("World", "!"), "Hello ")(_ + _) should be("Hello World!")
    }

  }

  "3.12 reverse" must {

    "reverse the list" in {
      List.reverse(List()) should be(List())
      List.reverse(List(1)) should be(List(1))
      List.reverse(List(3, 1, 7)) should be(List(7, 1, 3))
    }

  }

  "3.13 fold left via right" must {
    "work" in {
      List.foldLeftViaRight(List("foo", "bar"), "")(_ + _) should be("foobar")
    }
  }

  "3.14 append via fold left" must {
    "work" in {
      List.appendViaFoldLeft(List(), List()) should be(List())
      List.appendViaFoldLeft(List(1), List()) should be(List(1))
      List.appendViaFoldLeft(List(), List(1)) should be(List(1))
      List.appendViaFoldLeft(List(1), List(2)) should be(List(1, 2))
      List.appendViaFoldLeft(List(1, 2), List(3)) should be(List(1, 2, 3))
    }
  }

  "3.14 append via fold right" must {
    "work" in {
      List.appendViaFoldRight(List(), List()) should be(List())
      List.appendViaFoldRight(List(1), List()) should be(List(1))
      List.appendViaFoldRight(List(), List(1)) should be(List(1))
      List.appendViaFoldRight(List(1, 2), List(3)) should be(List(1, 2, 3))
    }
  }

  "3.15 concat" must {
    "flatten nested lists" in {
      List.concat(List(List(), List())) should be(List())
      List.concat(List(List(1), List())) should be(List(1))
      List.concat(List(List(), List(1))) should be(List(1))
      List.concat(List(List(1), List(2))) should be(List(1, 2))
      List.concat(List(List(1, 2, 3), List(4, 5, 6))) should be(List(1, 2, 3, 4, 5, 6))
      List.concat(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))) should be(List(1, 2, 3, 4, 5, 6, 7, 8, 9))
    }
  }

  "3.16 increment" must {
    "work" in {
      List.increment(List()) shouldEqual List()
      List.increment(List(1)) shouldEqual List(2)
      List.increment(List(1, 2)) shouldEqual List(2, 3)
      List.increment(List(0, 11, 3)) shouldEqual List(1, 12, 4)
    }
  }

  "3.17 double to string" must {
    "work" in {
      List.doubleToString(List()) shouldEqual List()
      List.doubleToString(List(1)) shouldEqual List("1.0")
      List.doubleToString(List(1, 2)) shouldEqual List("1.0", "2.0")
    }
  }

  "3.18 map" must {
    "apply the provided function to all elements of the list" in {
      List.map(List())(_.toString) shouldEqual List()
      List.map(List(1))(_.toString) shouldEqual List("1")
      List.map(List(3, 5, 7))(_.toString) shouldEqual List("3", "5", "7")
    }
  }

  "3.19 filter" must {
    "filter the elements of the list that do not conform to the provided predicate" in {
      List.filter(List[Int]())(x => x % 2 == 0) shouldEqual List()
      List.filter(List(1, 2, 3, 4, 5))(x => x % 2 == 0) shouldEqual List(2, 4)
      List.filter(List(1, 3, 1, 7, 5))(x => x % 2 == 0) shouldEqual List()
    }
  }

  "3.20 flatmap" must {
    "apply function that creates another list from each element and flattens into a final list" in {
      List.flatMap(List())(i => List(i, i)) shouldEqual List()
      List.flatMap(List(1, 2, 3))(i => List(i, i)) shouldEqual List(1, 1, 2, 2, 3, 3)
    }
  }

  "3.21 filter via flatmap" must {
    "work" in {
      List.filterViaFlatMap(List[Int]())(x => x % 2 == 0) shouldEqual List()
      List.filterViaFlatMap(List(1, 2, 3, 4, 5))(x => x % 2 == 0) shouldEqual List(2, 4)
      List.filterViaFlatMap(List(1, 3, 1, 7, 5))(x => x % 2 == 0) shouldEqual List()
    }
  }

  "3.22 addPairwise" must {
    "add corresponding elements" in {
      List.addPairwise(List(1, 2, 3), List(4, 5, 6)) shouldEqual List(5, 7, 9)
    }
  }

  "3.23 zipWith" must {
    "zip corresponding elements with the provided function" in {
      List.zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _) shouldEqual List(5, 7, 9)
    }
  }

  "zipWithIndex" must {
    "zip elements with index" in {

      List.zipWithIndex(List(), List()) shouldEqual List()

      List.zipWithIndex(List("a", "b"), List("d")) shouldEqual List(
        (0, "a", "d"),
      )

      List.zipWithIndex(List("a"), List("d", "e", "f")) shouldEqual List(
        (0, "a", "d"),
      )

      List.zipWithIndex(List("a", "b", "c"), List("d", "e", "f")) shouldEqual List(
        (0, "a", "d"),
        (1, "b", "e"),
        (2, "c", "f")
      )
    }
  }

  "3.24 hasSubsequence" must {
    "check if sub is subsequence of sup" in {
      List.hasSubsequence(List(), List()) shouldEqual false
      List.hasSubsequence(List(1), List(1)) shouldEqual true
      List.hasSubsequence(List(2), List(1)) shouldEqual false

      List.hasSubsequence(List(1, 2, 3, 4), List(1, 2)) shouldEqual true
      List.hasSubsequence(List(1, 2, 3, 4), List(2, 3)) shouldEqual true
      List.hasSubsequence(List(1, 2, 3, 4), List(4)) shouldEqual true
      List.hasSubsequence(List(1, 2, 3, 4), List(5)) shouldEqual false
      List.hasSubsequence(List(1, 2, 3, 4), List(4, 5)) shouldEqual false
      List.hasSubsequence(List(1, 2, 3, 4), List(1, 4)) shouldEqual false
      List.hasSubsequence(List(1, 2, 3, 4), List(1, 2, 3, 4, 5)) shouldEqual false
    }
  }



}
