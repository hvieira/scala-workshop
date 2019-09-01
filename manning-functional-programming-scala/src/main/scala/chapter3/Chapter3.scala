package chapter3

sealed trait List[+A] {
  def tail: List[A]
  def setHead[B >: A](elem: B): List[B]
  def drop(n: Int): List[A]
}
case object Nil extends List[Nothing] {
  def tail(): List[Nothing] = Nil

  def setHead[B >: Nothing](elem: B): List[B] = List(elem)

  override def drop(n: Int): List[Nothing] = Nil
}
case class Cons[+A](h: A, t: List[A]) extends List[A] {
  def tail(): List[A] = t

  def setHead[B >: A](elem: B): List[B] = Cons(elem, Cons(h, t))

  override def drop(n: Int): List[A] = n match {
    case i if i <= 0 => this
    case _ => t.drop(n - 1)
  }
}

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }



  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}