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

    "return a list all but the last element" in {
      List.init(List()) should be(List())
      List.init(List(1)) should be(List(1))
      List.init(List(1, 2)) should be(List(2))
      List.init(List(3, 5, 1)) should be(List(1))
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
      List.reverse(List(3, 1, 7)) should be(List(7,1,3))
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

}
