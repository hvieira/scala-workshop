package minesweeper

import minesweeper.errors._
import org.scalamock.scalatest.MockFactory
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{Failure, Success}

class GameBoardSpec extends AnyWordSpec with MockFactory with Matchers {

  "generate game board" must {

    val singleCoordGen = (_: Int,_:Int) => (_: Int) => Set(Coord(X(0), Y(0)))

    "validate the number of mines in accordance to game size" in {
      GameBoard.generateBoard(5, 5, 0, singleCoordGen) shouldEqual Failure(NoMinesInBoard)
      GameBoard.generateBoard(5, 5, -1, singleCoordGen) shouldEqual Failure(NoMinesInBoard)

      GameBoard.generateBoard(5, 5, 25, singleCoordGen) shouldEqual Failure(TooManyMinesForBoard)
      GameBoard.generateBoard(5, 5, 26, singleCoordGen) shouldEqual Failure(TooManyMinesForBoard)
    }

    "generate the game board with the number of mines specified" in {

      GameBoard.generateBoard(3, 3, 1, singleCoordGen) should ===(Success(
        GameBoard(
          board = Map(
            Coord(X(0), Y(0)) -> Mine(NotRevealed),
            Coord(X(0), Y(1)) -> Safe(NotRevealed, 0),
            Coord(X(0), Y(2)) -> Safe(NotRevealed, 0),

            Coord(X(1), Y(0)) -> Safe(NotRevealed, 0),
            Coord(X(1), Y(1)) -> Safe(NotRevealed, 0),
            Coord(X(1), Y(2)) -> Safe(NotRevealed, 0),

            Coord(X(2), Y(0)) -> Safe(NotRevealed, 0),
            Coord(X(2), Y(1)) -> Safe(NotRevealed, 0),
            Coord(X(2), Y(2)) -> Safe(NotRevealed, 0),
          ),
          w = 3, h = 3)
      ))

      val moreMines = (_: Int,_:Int) => (_: Int) => Set[Coord](
        Coord(X(0), Y(0)),
        Coord(X(0), Y(1)),
        Coord(X(1), Y(1))
      )
      GameBoard.generateBoard(2, 2, 3, moreMines) shouldEqual Success(GameBoard(
        board = Map(
          Coord(X(0), Y(0)) -> Mine(NotRevealed),
          Coord(X(0), Y(1)) -> Mine(NotRevealed),
          Coord(X(1), Y(0)) -> Safe(NotRevealed, 0),
          Coord(X(1), Y(1)) -> Mine(NotRevealed)
        ),
        w = 2, h = 2
      ))
    }
  }

//  "mine coord generator" must {
//
//    "gets all coordinates if asked for more mines than board cells" in {
//      val allCoords = Set(
//        Coord(X(0), Y(0)),
//        Coord(X(0), Y(1)),
//        Coord(X(1), Y(0)),
//        Coord(X(1), Y(1))
//      )
//
//      GameBoard.mineGenerator(2, 2)(4) shouldEqual allCoords
//      GameBoard.mineGenerator(2, 2)(10) shouldEqual allCoords
//      GameBoard.mineGenerator(2, 2)(10000) shouldEqual allCoords
//    }
//
//    "generate the required number of mines and should be distinct 2X2" in {
//      (1 to 1000).foreach { _ =>
//        val mineCoords = GameBoard.mineGenerator(2, 2)(1)
//        mineCoords.size shouldEqual 1
//        mineCoords.count(c => (c.x.value >= 0 && c.x.value < 2) && (c.y.value >= 0 && c.y.value < 2)) shouldEqual 1
//      }
//    }
//
//    "generate the required number of mines and should be distinct 5X5" in {
//      (1 to 1000).foreach { _ =>
//        val mineCoords = GameBoard.mineGenerator(5, 5)(5)
//        mineCoords.size shouldEqual 5
//        mineCoords.count(c => (c.x.value >= 0 && c.x.value < 5) && (c.y.value >= 0 && c.y.value < 5)) shouldEqual 5
//      }
//    }
//
//    "generate the required number of mines and should be distinct 50X50" in {
//      (1 to 50).foreach { _ =>
//        val mineCoords = GameBoard.mineGenerator(50, 50)(100)
//        mineCoords.size shouldEqual 100
//        mineCoords.count(c => (c.x.value >= 0 && c.x.value < 50) && (c.y.value >= 0 && c.y.value < 50)) shouldEqual 100
//      }
//    }
//
//  }


  "Game board" when {

    "picking a cell" should {

      "validate coordinates - 0-index based" in {
        val squareThreeByThree = GameBoard(board = Map(), w = 3, h = 3)

        val rectangleThreeByFive = GameBoard(board = Map(), w = 3, h = 5)

        squareThreeByThree.pick(Coord(X(-1), Y(-1))) shouldEqual Left(OutOfBoundsPick(Coord(X(-1), Y(-1))))
        squareThreeByThree.pick(Coord(X(0), Y(-1))) shouldEqual Left(OutOfBoundsPick(Coord(X(0), Y(-1))))
        squareThreeByThree.pick(Coord(X(-1), Y(0))) shouldEqual Left(OutOfBoundsPick(Coord(X(-1), Y(0))))

        squareThreeByThree.pick(Coord(X(3), Y(3))) shouldEqual Left(OutOfBoundsPick(Coord(X(3), Y(3))))
        squareThreeByThree.pick(Coord(X(3), Y(2))) shouldEqual Left(OutOfBoundsPick(Coord(X(3), Y(2))))
        squareThreeByThree.pick(Coord(X(2), Y(3))) shouldEqual Left(OutOfBoundsPick(Coord(X(2), Y(3))))

        squareThreeByThree.pick(Coord(X(0), Y(100))) shouldEqual Left(OutOfBoundsPick(Coord(X(0), Y(100))))


        rectangleThreeByFive.pick(Coord(X(-1), Y(-1))) shouldEqual Left(OutOfBoundsPick(Coord(X(-1), Y(-1))))
        rectangleThreeByFive.pick(Coord(X(0), Y(-1))) shouldEqual Left(OutOfBoundsPick(Coord(X(0), Y(-1))))
        rectangleThreeByFive.pick(Coord(X(-1), Y(0))) shouldEqual Left(OutOfBoundsPick(Coord(X(-1), Y(0))))

        rectangleThreeByFive.pick(Coord(X(3), Y(5))) shouldEqual Left(OutOfBoundsPick(Coord(X(3), Y(5))))
        rectangleThreeByFive.pick(Coord(X(3), Y(4))) shouldEqual Left(OutOfBoundsPick(Coord(X(3), Y(4))))
        rectangleThreeByFive.pick(Coord(X(2), Y(5))) shouldEqual Left(OutOfBoundsPick(Coord(X(2), Y(5))))

        rectangleThreeByFive.pick(Coord(X(0), Y(5))) shouldEqual Left(OutOfBoundsPick(Coord(X(0), Y(5))))
        rectangleThreeByFive.pick(Coord(X(100), Y(0))) shouldEqual Left(OutOfBoundsPick(Coord(X(100), Y(0))))
      }

      "reveal all immediate safe  neighbours" in {
        val game = GameBoard(
          board = Map(
            Coord(X(0), Y(0)) -> Safe(NotRevealed, 0),
            Coord(X(0), Y(1)) -> Safe(NotRevealed, 0),
            Coord(X(0), Y(2)) -> Safe(NotRevealed, 0),

            Coord(X(1), Y(0)) -> Safe(NotRevealed, 0),
            Coord(X(1), Y(1)) -> Safe(NotRevealed, 0),
            Coord(X(1), Y(2)) -> Safe(NotRevealed, 0),

            Coord(X(2), Y(0)) -> Safe(NotRevealed, 0),
            Coord(X(2), Y(1)) -> Safe(NotRevealed, 0),
            Coord(X(2), Y(2)) -> Safe(NotRevealed, 0),
          ),
          w = 3, h = 3
        )

        val allRevealed = game.copy(board = game.board.view.mapValues(_ => Safe(Revealed, 0)).toMap)

        game.pick(Coord(X(1), Y(1))) shouldEqual Right(allRevealed)
      }

      "reveal all safe  neighbours" in {
        val game = GameBoard(
          board = Map(
            Coord(X(0), Y(0)) -> Safe(NotRevealed, 0),
            Coord(X(0), Y(1)) -> Safe(NotRevealed, 0),
            Coord(X(0), Y(2)) -> Safe(NotRevealed, 0),

            Coord(X(1), Y(0)) -> Safe(NotRevealed, 0),
            Coord(X(1), Y(1)) -> Safe(NotRevealed, 0),
            Coord(X(1), Y(2)) -> Safe(NotRevealed, 0),

            Coord(X(2), Y(0)) -> Safe(NotRevealed, 0),
            Coord(X(2), Y(1)) -> Safe(NotRevealed, 0),
            Coord(X(2), Y(2)) -> Safe(NotRevealed, 0),
          ),
          w = 3, h = 3
        )

        val allRevealed = game.copy(board = game.board.view.mapValues(_ => Safe(Revealed, 0)).toMap)

        game.pick(Coord(X(1), Y(1))) shouldEqual Right(allRevealed)
      }

      "reveal all safe  neighbours in a bigger rectangular board" in {
        val game = GameBoard(
          board = Map(
            Coord(X(0), Y(0)) -> Safe(NotRevealed, 0),
            Coord(X(0), Y(1)) -> Safe(NotRevealed, 0),
            Coord(X(0), Y(2)) -> Safe(NotRevealed, 0),
            Coord(X(0), Y(3)) -> Safe(NotRevealed, 0),

            Coord(X(1), Y(0)) -> Safe(NotRevealed, 0),
            Coord(X(1), Y(1)) -> Safe(NotRevealed, 0),
            Coord(X(1), Y(2)) -> Safe(NotRevealed, 0),
            Coord(X(1), Y(3)) -> Safe(NotRevealed, 0),

            Coord(X(2), Y(0)) -> Safe(NotRevealed, 0),
            Coord(X(2), Y(1)) -> Safe(NotRevealed, 0),
            Coord(X(2), Y(2)) -> Safe(NotRevealed, 0),
            Coord(X(2), Y(3)) -> Safe(NotRevealed, 0),
          ),
          w = 4, h = 3
        )

        val allRevealed = game.copy(board = game.board.view.mapValues(_ => Safe(Revealed, 0)).toMap)

        game.pick(Coord(X(0), Y(0))) shouldEqual Right(allRevealed)
      }

      "be a no-op if picked is already revealed" in {
        val game = GameBoard(
          board = Map(
            Coord(X(0), Y(0)) -> Safe(Revealed, 0),
            Coord(X(0), Y(1)) -> Safe(NotRevealed, 0),
            Coord(X(0), Y(2)) -> Safe(NotRevealed, 0),

            Coord(X(1), Y(0)) -> Safe(NotRevealed, 0),
            Coord(X(1), Y(1)) -> Safe(NotRevealed, 0),
            Coord(X(1), Y(2)) -> Safe(NotRevealed, 0),

            Coord(X(2), Y(0)) -> Safe(Revealed, 0),
            Coord(X(2), Y(1)) -> Safe(NotRevealed, 0),
            Coord(X(2), Y(2)) -> Safe(NotRevealed, 0),
          ),
          w = 3, h = 3
        )

        game.pick(Coord(X(0), Y(0))) shouldEqual Right(game)
        game.pick(Coord(X(2), Y(0))) shouldEqual Right(game)
      }

    }

  }

}
