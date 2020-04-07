package katas

object BinarySearch {

  def search(seq: Seq[Int], lookingFor: Int): Option[Int] = {

    @scala.annotation.tailrec
    def binarySearch(seq: IndexedSeq[Int], lookingFor: Int, leftBound: Int, rightBound: Int): Option[Int] = {
      rightBound - leftBound match {
        case d if d < 0 => None
        case 0 =>
          if (seq(leftBound) == lookingFor) Some(leftBound) else None
        case _ =>
          val chop = Math.floor((rightBound + leftBound) / 2).toInt
          if (lookingFor > seq(chop)) binarySearch(seq, lookingFor, chop + 1, rightBound)
          else binarySearch(seq, lookingFor, leftBound, chop)
      }

    }
    val indexed = seq.toIndexedSeq
    binarySearch(indexed, lookingFor, 0, indexed.length - 1)
  }

}
