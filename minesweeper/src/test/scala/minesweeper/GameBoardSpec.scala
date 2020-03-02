package minesweeper

import minesweeper.errors._
import org.scalamock.scalatest.MockFactory
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{Failure, Success}

class GameBoardSpec extends AnyWordSpec with MockFactory with Matchers {

  "generate game board" must {

    val singleCoordGen = (_: Int) => Set(Coord(X(0), Y(0)))

    "validate the number of mines in accordance to game size" in {
      GameBoard.generateBoard(5,5,0, singleCoordGen) shouldEqual Failure(NoMinesInBoard)
      GameBoard.generateBoard(5,5,-1, singleCoordGen) shouldEqual Failure(NoMinesInBoard)

      GameBoard.generateBoard(5,5,25, singleCoordGen) shouldEqual Failure(TooManyMinesForBoard)
      GameBoard.generateBoard(5,5,26, singleCoordGen) shouldEqual Failure(TooManyMinesForBoard)
    }

    "generate the game board with the number of mines specified" in {

      GameBoard.generateBoard(3,3,1, singleCoordGen) should ===(Success(
        GameBoard(
          Map(
            Coord(X(0), Y(0)) -> Mine(NotRevealed),
            Coord(X(0), Y(1)) -> Safe(NotRevealed, 0),
            Coord(X(0), Y(2)) -> Safe(NotRevealed, 0),

            Coord(X(1), Y(0)) -> Safe(NotRevealed, 0),
            Coord(X(1), Y(1)) -> Safe(NotRevealed, 0),
            Coord(X(1), Y(2)) -> Safe(NotRevealed, 0),

            Coord(X(2), Y(0)) -> Safe(NotRevealed, 0),
            Coord(X(2), Y(1)) -> Safe(NotRevealed, 0),
            Coord(X(2), Y(2)) -> Safe(NotRevealed, 0),
          )
        )
      ))

      val moreMines = (_: Int) => Set[Coord](
        Coord(X(0), Y(0)),
        Coord(X(0), Y(1)),
        Coord(X(1), Y(1))
      )
      GameBoard.generateBoard(2,2,3, moreMines) should ===(Success(
        GameBoard(
          Map(
            Coord(X(0), Y(0)) -> Mine(NotRevealed),
            Coord(X(0), Y(1)) -> Mine(NotRevealed),
            Coord(X(1), Y(0)) -> Safe(NotRevealed, 0),
            Coord(X(1), Y(1)) -> Mine(NotRevealed)
          )
        )
      ))
    }

  "mine coord generator" must {

    "generate the required number of mines and should be distinct 2X2" in {
      (1 to 1000).forall { _ =>
        val mineCoords = GameBoard.mineGenerator(2,2)(1)
        mineCoords.count(c => (c.x.value >= 0 && c.x.value < 2) && (c.y.value >= 0 && c.y.value < 2)) == 1
      }
    }

    "generate the required number of mines and should be distinct 5X5" in {
      (1 to 1000).forall { _ =>
        val mineCoords = GameBoard.mineGenerator(5,5)(5)
        mineCoords.count(c => (c.x.value >= 0 && c.x.value < 5) && (c.y.value >= 0 && c.y.value < 5)) == 5
      }
    }

    "generate the required number of mines and should be distinct 50X50" in {
      (1 to 1000).forall { _ =>
        val mineCoords = GameBoard.mineGenerator(50,50)(100)
        mineCoords.count(c => (c.x.value >= 0 && c.x.value < 50) && (c.y.value >= 0 && c.y.value < 50)) == 100
      }
    }

  }


  }

}
