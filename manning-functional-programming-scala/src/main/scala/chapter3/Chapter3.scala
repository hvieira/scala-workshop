package chapter3

sealed trait List[+A] {
  def tail: List[A]

  def setHead[B >: A](elem: B): List[B]

  def drop(n: Int): List[A]

  def dropWhile[B >: A](f: B => Boolean): List[B]
}

case object Nil extends List[Nothing] {
  def tail(): List[Nothing] = Nil

  def setHead[B >: Nothing](elem: B): List[B] = List(elem)

  def drop(n: Int): List[Nothing] = Nil

  def dropWhile[B >: Nothing](f: B => Boolean): List[B] = Nil
}

case class Cons[+A](h: A, t: List[A]) extends List[A] {
  def tail(): List[A] = t

  def setHead[B >: A](elem: B): List[B] = Cons(elem, Cons(h, t))

  def drop(n: Int): List[A] = n match {
    case i if i <= 0 => this
    case _ => t.drop(n - 1)
  }

  def dropWhile[B >: A](f: B => Boolean): List[B] = if (f(h)) t.dropWhile(f) else this
}

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(ph, t) => Cons(h, Cons(ph, t))
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, _) if n <= 0 => l
    case Cons(_, t) if n > 0 => drop(t, n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, Nil) => List(h)
      case Cons(h, t) => init(t)
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, count) => count + 1)

}