package minesweeper

import minesweeper.errors.{NoMinesInBoard, TooManyMinesForBoard}

import scala.util.{Failure, Random, Success, Try}

case class GameBoard(board: Map[Coord, BoardCell]) {

}

object GameBoard {
  def generateBoard(w: Int,
                    h: Int,
                    nMines: Int,
                    mineCoordsGenerator: Int => Set[Coord]): Try[GameBoard] = {

    if (nMines <= 0)
      Failure(NoMinesInBoard)
    else if (nMines >= (w * h))
      Failure(TooManyMinesForBoard)
    else {
      val initialMatrix: Map[Coord, BoardCell] =
        (for {
          x <- 0 until w
          y <- 0 until h
        } yield (
          Coord(X(x), Y(y)),
          Safe(
            NotRevealed,
            0
          )
        )).toMap

      Success(
        GameBoard(
          mineCoordsGenerator(nMines).foldLeft(initialMatrix) {
            case (board, mineCoord) => board.updated(mineCoord, Mine(NotRevealed))
          }
        )
      )
    }

  }

  val mineGenerator: (Int, Int) => Int => Set[Coord] = (w: Int, h: Int) => (nMines: Int) => {
    val allCoords = (for {
      x <- 0 until w
      y <- 0 until h
    } yield Coord(X(x), Y(y))).toList

    val selected = for {
      max <- allCoords.size.to(allCoords.size - nMines, -1)
    } yield Random.nextInt(max)

    selected.map(allCoords(_)).toSet
  }
}
