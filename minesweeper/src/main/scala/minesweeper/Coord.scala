package minesweeper

final case class X(value: Int) extends AnyVal
final case class Y(value: Int) extends AnyVal

case class Coord(x: X, y: Y)
