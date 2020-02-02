package chapter4

import org.scalatest.{Matchers, WordSpec}

class OptionSpec extends WordSpec with Matchers {

  "Option" must {
    "know how to apply a map function" in {
      val fPlusOne = (x:Int) => x + 1
      None.map(fPlusOne) shouldEqual None
      Some(1).map(fPlusOne) shouldEqual Some(2)
      Some(-1).map(fPlusOne) shouldEqual Some(0)
    }

    "know how to apply a flatMap function" in {
      val fPlusOneIfNotZero = (x:Int) => if (x == 0) None else Some(x + 1)
      None.flatMap(fPlusOneIfNotZero) shouldEqual None
      Some(0).flatMap(fPlusOneIfNotZero) shouldEqual None
      Some(1).flatMap(fPlusOneIfNotZero) shouldEqual Some(2)
      Some(-1).flatMap(fPlusOneIfNotZero) shouldEqual Some(0)
    }

    "know how to get or use a default if None" in {
      None.getOrElse(1) shouldEqual 1
      Some(2).getOrElse(1) shouldEqual 2
    }

    "know how to to use an alternative Option if None" in {
      None.orElse(Some(1)) shouldEqual Some(1)
      Some(2).orElse(Some(1)) shouldEqual Some(2)
    }

    "know how to filter according to a predicate" in {
      val predicate: Int => Boolean = x => x == 0
      None.filter(predicate) shouldEqual None
      Some(1).filter(predicate) shouldEqual None
      Some(0).filter(predicate) shouldEqual Some(0)
    }
  }

}
