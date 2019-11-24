package chapter3

import org.scalatest.{Matchers, WordSpec}

class TreeSpec extends WordSpec with Matchers {

  "Tree" must {
    "know the size" in {
      Tree.size(Leaf("d")) shouldEqual 1

      Tree.size(Branch(Leaf("d"), Leaf("a"))) shouldEqual 3

      Tree.size(
        Branch(
          Branch(
            Leaf("d"),
            Leaf("a")
          ),
          Leaf("z")
        )
      ) shouldEqual 5

      Tree.size(
        Branch(
          Branch(
            Leaf("d"),
            Leaf("a")
          ),
          Branch(
            Leaf("y"),
            Leaf("z")
          )
        )
      ) shouldEqual 7
    }

    "compute maximum in a tree of ints" in {
      Tree.maximum(Leaf(0)) shouldEqual 0

      Tree.maximum(Branch(Leaf(0), Leaf(1))) shouldEqual 1
      Tree.maximum(Branch(Leaf(1), Leaf(0))) shouldEqual 1
      Tree.maximum(Branch(Leaf(1), Leaf(1))) shouldEqual 1

      Tree.maximum(
        Branch(
          Branch(
            Leaf(0),
            Leaf(1)
          ),
          Leaf(2)
        )
      ) shouldEqual 2

      Tree.maximum(
        Branch(
          Branch(
            Leaf(3),
            Leaf(2)
          ),
          Branch(
            Leaf(2),
            Leaf(4)
          )
        )
      ) shouldEqual 4
    }

    "compute depth" in {
      Tree.depth(Leaf(0)) shouldEqual 1

      Tree.depth(Branch(Leaf(0), Leaf(1))) shouldEqual 2

      Tree.depth(
        Branch(
          Branch(
            Leaf(0),
            Leaf(1)
          ),
          Leaf(2)
        )
      ) shouldEqual 3

      Tree.depth(
        Branch(
          Branch(
            Leaf(3),
            Leaf(2)
          ),
          Branch(
            Leaf(2),
            Leaf(4)
          )
        )
      ) shouldEqual 3

      Tree.depth(
        Branch(
          Branch(
            Leaf(0),
            Branch(
              Leaf(0),
              Branch(
                Branch(
                  Leaf(0),
                  Leaf(1)
                ),
                Leaf(1)
              )
            )
          ),
          Leaf(2)
        )
      ) shouldEqual 6
    }

    "know how to apply a map function to its values" in {
      val func = (i: Int) => i.toString
      Tree.map(Leaf(1), func) shouldEqual Leaf("1")

      Tree.map(Branch(Leaf(1), Leaf(2)), func) shouldEqual Branch(Leaf("1"), Leaf("2"))

      Tree.map(
        Branch(
          Branch(
            Leaf(1),
            Leaf(2)
          ),
          Branch(
            Leaf(3),
            Leaf(4)
          )
        )
        , func) shouldEqual Branch(
        Branch(
          Leaf("1"),
          Leaf("2")
        ),
        Branch(
          Leaf("3"),
          Leaf("4")
        )
      )
    }

  }

}
