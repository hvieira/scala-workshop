package minesweeper

import minesweeper.errors.{NoMinesInBoard, OutOfBoundsPick, TooManyMinesForBoard}

import scala.util.{Failure, Random, Success, Try}

case class GameBoard(board: Map[Coord, BoardCell], w: Int, h: Int) {
  def pick(picked: Coord): Either[OutOfBoundsPick, GameBoard] =
    if (areCoordWithinBounds(picked)) Right(reveal(picked)) else Left(OutOfBoundsPick(picked))

  // TODO improve this - should not need to call .value on X or Y
  private def areCoordWithinBounds(coord: Coord): Boolean =
    coord.x.value >= 0 && coord.x.value < h && coord.y.value >= 0 && coord.y.value < w

  private def validNeighbourCoordinates(coord: Coord): Set[Coord] = {
    (for {
      x <- coord.x.value - 1 to coord.x.value + 1
      y <- coord.y.value - 1 to coord.y.value + 1
    } yield Coord(X(x), Y(y)))
      .filter(areCoordWithinBounds)
      .filterNot(_ == coord)
      .toSet
  }

  private def reveal(coord: Coord): GameBoard = {

    @scala.annotation.tailrec
    def discoverRevealable(candidatesToReveal: Set[Coord], toReveal: Set[Coord] = Set()): Set[Coord] = {
      candidatesToReveal.toList match {
        case c :: _ =>
          val newToReveal = toReveal + c
          val thisCoordNeighbours = validNeighbourCoordinates(c)
          val thisCoordNeighboursExceptAlreadyToBeRevealed = thisCoordNeighbours.filterNot(newToReveal.contains)
          discoverRevealable((candidatesToReveal - c) ++ thisCoordNeighboursExceptAlreadyToBeRevealed, newToReveal)
        case Nil => toReveal
      }
    }

    if (board(coord).cellState == Revealed)
      this
    else {
      val toReveal = discoverRevealable(Set(coord))
      toReveal.foldLeft(this) {
        case (b, c) => b.copy(board = b.board.updated(c, b.board(c).reveal))
      }
    }

  }
}


object GameBoard {
  type MineCoordinatesGenerator = (Int, Int) => Int => Set[Coord]

  def generateBoard(w: Int,
                    h: Int,
                    nMines: Int,
                    mineCoordsGenerator: MineCoordinatesGenerator): Try[GameBoard] = {

    if (nMines <= 0)
      Failure(NoMinesInBoard)
    else if (nMines >= (w * h))
      Failure(TooManyMinesForBoard)
    else {
      val initialMatrix: Map[Coord, BoardCell] =
        (for {
          x <- 0 until w
          y <- 0 until h
        } yield Coord(X(x), Y(y)) -> Safe(NotRevealed, 0)).toMap

      Success(
        GameBoard(
          board = mineCoordsGenerator(w, h)(nMines).foldLeft(initialMatrix) {
            case (board, mineCoord) => board.updated(mineCoord, Mine(NotRevealed))
          },
          w = w,
          h = h
        )
      )
    }

  }

  //  val mineGenerator: (Int, Int) => Int => Set[Coord] = (w: Int, h: Int) => (nMines: Int) => {
  //
  //    @scala.annotation.tailrec
  //    def selectRandomCoords(remainingCoord: List[Coord], selected: Set[Coord]): Set[Coord] = {
  //      if (selected.size == nMines)
  //        selected
  //      else {
  //        val thisOne = remainingCoord(Random.nextInt(remainingCoord.length))
  //        selectRandomCoords(
  //          remainingCoord.diff(List(selected)),
  //          selected.incl(thisOne)
  //        )
  //      }
  //    }
  //
  //    val allCoords = (for {
  //      x <- 0 until w
  //      y <- 0 until h
  //    } yield Coord(X(x), Y(y))).toList
  //
  //
  //    if (nMines >= w * h) allCoords.toSet else selectRandomCoords(allCoords, Set())
  //  }
}
