package minesweeper.errors

case object NoMinesInBoard extends RuntimeException("No mines in board")
case object TooManyMinesForBoard extends RuntimeException("Too many mines for board")
