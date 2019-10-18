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

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((acc, elem) => acc.setHead(elem))

  def foldRightViaLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((b, a) => f(a, b))

  def foldLeftViaRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  /**
    * List("foo", "bar", "z") | AGGR "acc" | f concat
    * (z, ID)                                         -> B => ID(concat(B,z))
    * (bar, B => ID(concat(B,z)))                     -> B => ID(concat(pB,z))(concat(B,bar))
    * (foo, B => ID(concat(pB,z))(concat(B,bar)))     -> B => ID(concat(ppB,z))(concat(pB,bar))(concat(B,foo))
    *
    * feed in "acc"
    * ID(concat(ppB,z))(concat(pB,bar))(concat(acc,foo))
    * ID(concat(ppB,z))(concat(accfoo,bar))
    * ID(concat(accfoobar,z))
    * ID(accfoobarz)
    * accfoobarz
    */

  def appendViaFoldLeft[A](appendTo: List[A], more: List[A]): List[A] =
    List.foldLeft(List.reverse(appendTo), more)((b, a) => Cons(a, b))

  def appendViaFoldRight[A](appendTo: List[A], more: List[A]): List[A] =
    List.foldRight(appendTo, more)((a, b) => Cons(a, b))

  def concat[A](l: List[List[A]]): List[A] =
    List.foldLeft(l, List[A]())((b, a) => List.append(b, a))

  def increment(l: List[Int]): List[Int] =
    l match {
      case Nil => Nil
      case Cons(h, t) => Cons(h + 1, increment(t))
    }

  def doubleToString(l: List[Double]): List[String] =
    l match {
      case Nil => Nil
      case Cons(h, t) => Cons(h.toString, doubleToString(t))
    }

  def map[A, B](as: List[A])(f: A => B): List[B] =
    as match {
      case Nil => Nil
      case Cons(h, t) => Cons(f(h), map(t)(f))
    }

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    List.foldRightViaLeft(as, List[A]())((a, b) => if (f(a)) setHead(b, a) else b)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else List())

  def addPairwise(a: List[Int], b: List[Int]): List[Int] =
    (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
    }

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] =
    (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

  def contains[A](l: List[A], a: A): Boolean =
    l match {
      case Nil => false
      case Cons(h, t) => if (h == a) true else contains(t, a)
    }

  def zipWithIndex[A, B](a: List[A], b: List[B]): List[(Int, A, B)] = {

    def inner[C, D](a: List[C], b: List[D], i: Int): List[(Int, C, D)] =
      (a, b) match {
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case (Cons(h1, t1), Cons(h2, t2)) => Cons((i, h1, h2), inner(t1, t2, i + 1))
      }

    inner(a, b, 0)
  }


  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {

    def inner[B](sup: List[B], sub: List[B], foundSeq: Boolean, inSeq: Boolean): Boolean = {
      (sup, sub) match {
        case (_, Nil) => foundSeq
        case (Nil, _) => false
        case (Cons(h1, t1), Cons(h2, t2)) if !inSeq && !foundSeq && h1 == h2  => inner(t1, t2, foundSeq = true, inSeq = true)
        case (Cons(h1, _), Cons(h2, _)) if !inSeq && foundSeq && h1 == h2     => false
        case (Cons(h1, t1), Cons(h2, t2)) if inSeq && h1 == h2                => inner(t1, t2, foundSeq = true, inSeq = true)
        case _                                                                => inner(tail(sup), sub, foundSeq = foundSeq, inSeq = false)
      }
    }
    inner(sup, sub, foundSeq = false, inSeq = false)
  }

  // from the book
  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
    case (_,Nil) => true
    case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }

  @annotation.tailrec
  def hasSubsequenceAnswers[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(h,t) => hasSubsequenceAnswers(t, sub)
  }

}