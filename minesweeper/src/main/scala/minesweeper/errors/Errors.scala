package minesweeper.errors

import minesweeper.Coord

case object NoMinesInBoard extends RuntimeException("No mines in board")
case object TooManyMinesForBoard extends RuntimeException("Too many mines for board")

case class OutOfBoundsPick(coord: Coord) extends RuntimeException(s"Coordinates $coord is out of bounds for the board")
