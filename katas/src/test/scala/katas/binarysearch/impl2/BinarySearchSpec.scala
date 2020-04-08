package katas.binarysearch.impl2

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BinarySearchSpec extends AnyWordSpec with Matchers {

  "BinarySearch" when {

    "searching on an empty collection" should {

      "return None" in {
        BinarySearch.search(Seq[Int](), 0) should === (None)
        BinarySearch.search(List[Int](), 0) should === (None)
        BinarySearch.search(Vector[Int](), 0) should === (None)
      }

    }

    "searching on an single element collection" should {

      "return the element if it is the one being searched for" in {
        BinarySearch.search(Seq(1), 1) should === (Some(1))
      }

      "return None if it is not in the collection" in {
        BinarySearch.search(Seq(9), 1) should === (None)
      }

    }

    "searching on a 3 element ordered collection" should {

      val collection = Seq(1, 2, 3)

      "return the the element being searched if it is in the collection" in {
        BinarySearch.search(collection, 1) should === (Some(1))
        BinarySearch.search(collection, 2) should === (Some(2))
        BinarySearch.search(collection, 3) should === (Some(3))
      }

      "return None if it is the one being searched for is not in the collection" in {
        BinarySearch.search(collection, 10) should === (None)
        BinarySearch.search(collection, 6) should === (None)
        BinarySearch.search(collection, 0) should === (None)
        BinarySearch.search(collection, 4) should === (None)
      }
    }

    "searching on a 6 element ordered collection" should {

      val collection = Seq(1, 2, 3, 4, 5, 6)

      "return the element being searched if it is in the collection" in {
        BinarySearch.search(collection, 1) should === (Some(1))
        BinarySearch.search(collection, 2) should === (Some(2))
        BinarySearch.search(collection, 3) should === (Some(3))
        BinarySearch.search(collection, 4) should === (Some(4))
        BinarySearch.search(collection, 5) should === (Some(5))
        BinarySearch.search(collection, 6) should === (Some(6))
      }

      "return None if it is the one being searched for is not in the collection" in {
        BinarySearch.search(collection, 0) should === (None)
        BinarySearch.search(collection, 7) should === (None)
      }
    }

    "searching on a 11 element ordered collection" should {

      val collection = Seq(1,2,3,4,5,6,7,8,9,10,11)

      "return the element being searched if it is in the collection" in {
        BinarySearch.search(collection, 1) should === (Some(1))
        BinarySearch.search(collection, 3) should === (Some(3))
        BinarySearch.search(collection, 5) should === (Some(5))
        BinarySearch.search(collection, 6) should === (Some(6))
        BinarySearch.search(collection, 10) should === (Some(10))
        BinarySearch.search(collection, 11) should === (Some(11))
      }

      "return None if it is the one being searched for is not in the collection" in {
        BinarySearch.search(collection, 0) should === (None)
        BinarySearch.search(collection, 12) should === (None)
      }
    }

    "searching on a sparse ordered collection" should {

      "return the element being searched if it is in the collection" in {
        BinarySearch.search(Seq(0,10), 0) should === (Some(0))
        BinarySearch.search(Seq(0,10), 10) should === (Some(10))

        BinarySearch.search(Seq(0,1,9), 0) should === (Some(0))
        BinarySearch.search(Seq(0,1,9), 1) should === (Some(1))
        BinarySearch.search(Seq(0,1,9), 9) should === (Some(9))

        BinarySearch.search(Seq(0,1,5,7,9,100), 0) should === (Some(0))
        BinarySearch.search(Seq(0,1,5,7,9,100), 1) should === (Some(1))
        BinarySearch.search(Seq(0,1,5,7,9,100), 5) should === (Some(5))
        BinarySearch.search(Seq(0,1,5,7,9,100), 7) should === (Some(7))
        BinarySearch.search(Seq(0,1,5,7,9,100), 9) should === (Some(9))
        BinarySearch.search(Seq(0,1,5,7,9,100), 100) should === (Some(100))
      }

      "return None if it is the one being searched for is not in the collection" in {
        BinarySearch.search(Seq(0,1,5,7,9,100), 8) should === (None)
        BinarySearch.search(Seq(0,1,5,7,9,100), -1) should === (None)
        BinarySearch.search(Seq(0,1,5,7,9,100), 3) should === (None)
        BinarySearch.search(Seq(0,1,5,7,9,100), 4) should === (None)
      }
    }

    "searching on a big ordered collection" should {

      val collection = 3 to 99999 by 3

      "be able to find elements" in {
        BinarySearch.search(collection, 3) should === (Some(3))
        BinarySearch.search(collection, 300) should === (Some(300))
        BinarySearch.search(collection, 3000) should === (Some(3000))
        BinarySearch.search(collection, 9000) should === (Some(9000))
      }

      "return None if it is the one being searched for is not in the collection" in {
        BinarySearch.search(collection, -1) should === (None)
        BinarySearch.search(collection, 0) should === (None)
        BinarySearch.search(collection, 5) should === (None)
        BinarySearch.search(collection, 99998) should === (None)
      }
    }

  }

}
