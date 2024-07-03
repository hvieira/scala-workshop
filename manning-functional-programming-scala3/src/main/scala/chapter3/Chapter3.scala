package chapter3

sealed trait MyList[+A]
case object Nil extends MyList[Nothing]
case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {
  def sum(ints: MyList[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: MyList[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail*))

  def tail[A](l: MyList[A]): MyList[A] = l match {
    case Nil => Nil
    case Cons(h, t) => t
  }

  def setHead[A](item: A, list: MyList[A]): MyList[A] = Cons(item, list)

  def drop[A](list: MyList[A], n: Int): MyList[A] = (n, list) match {
    case (_, Nil) => Nil
    case (0, l) => l
    case (x, Cons(h, t)) => drop(t, x-1)
  }

  def dropWhile[A](list: MyList[A], predicate: A => Boolean): MyList[A] = list match {
    case Nil => Nil
    case Cons(h, t) if predicate(h) => dropWhile(t, predicate)
    case l => l
  }

  def init[A](list: MyList[A]): MyList[A] = list match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  // def reverseList[A](l: MyList[A]): MyList[A] = {
  //   l match {
  //     case Nil     => Nil
  //     case MyList(h) => MyList(h)
  //     case h :: t  => Cons(reverseList(t), MyList(h))
  //   }
  // }

}
