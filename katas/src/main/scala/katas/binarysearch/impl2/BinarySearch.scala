package katas.binarysearch.impl2

object BinarySearch {

  import scala.math.Ordering.Implicits._

  @scala.annotation.tailrec
  def search[A : Ordering](seq: Seq[A], lookingFor: A): Option[A] =
    seq.toList match {
      case Nil =>
        None

      case h :: Nil =>
        if (h == lookingFor) Some(h) else None

      case collection =>
        val chop = Math.floor((seq.size - 1) / 2).toInt
        if (lookingFor > collection(chop)) search(collection.slice(chop + 1, collection.size), lookingFor)
        else search(collection.slice(0, chop + 1), lookingFor)
    }

}
