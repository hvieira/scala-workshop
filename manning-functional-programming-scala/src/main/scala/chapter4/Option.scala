package chapter4

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](ob: => Option[B]): Option[B]
  def filter(f: A => Boolean): Option[A]
}
case class Some[+A](v: A) extends Option[A] {
  override def map[B](f: A => B): Option[B] =
    Some(f(v))

  override def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) match {
      case Some(s) => s
      case None => None
    }

  override def getOrElse[B >: A](default: => B): B =
    v

  override def orElse[B >: A](ob: => Option[B]): Option[B] =
    this

  override def filter(f: A => Boolean): Option[A] = if(f(v)) this else None
}
case object None extends Option[Nothing] {
  override def map[B](f: Nothing => B): Option[B] =
    None

  override def flatMap[B](f: Nothing => Option[B]): Option[B] =
    None

  override def getOrElse[B >: Nothing](default: => B): B =
    default

  override def orElse[B >: Nothing](ob: => Option[B]): Option[B] =
    ob

  override def filter(f: Nothing => Boolean): Option[Nothing] =
    None
}

object Option {
  def apply[A](a: A): Option[A] = if (null == a) Some(a) else None
}
