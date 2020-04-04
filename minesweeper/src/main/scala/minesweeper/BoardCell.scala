package minesweeper

sealed trait BoardCellState
case object NotRevealed extends BoardCellState
case object Revealed extends BoardCellState
case object Flagged extends BoardCellState

sealed trait BoardCell {
  val cellState: BoardCellState

  def reveal: BoardCell
}

final case class Mine(cellState: BoardCellState) extends BoardCell {
  override def reveal: BoardCell = Mine(Revealed)
}
final case class Safe(cellState: BoardCellState, number: Int) extends BoardCell {
  override def reveal: BoardCell = Safe(Revealed, number)
}


