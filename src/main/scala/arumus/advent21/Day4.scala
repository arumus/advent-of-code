package arumus.advent21

import scala.annotation.tailrec

object Day4 {
  @tailrec
  def columnWon(board: Board, row: Int = 0, col: Int = 0, won: Boolean = true): Boolean =
    if col == board.grid.head.length then false
    else if row == board.grid.length && won then won
    else if row == board.grid.length then columnWon(board, col = col + 1)
    else columnWon(board, row = row + 1, col = col, won & board.grid(row)(col).marked)

  @tailrec
  def rowWon(board: Board, row: Int = 0, col: Int = 0, won: Boolean = true): Boolean =
    if row == board.grid.length then false
    else if col == board.grid(row).length && won then true
    else if col == board.grid(row).length then rowWon(board, row = row + 1)
    else rowWon(board, row = row, col = col + 1, won & board.grid(row)(col).marked)

  def calculateScore(board: Option[Board], lastDrawn: Int): Int =
    board.fold(0)(wb => wb.grid.map(_.filter(!_.marked).map(_.value).sum).sum) * lastDrawn

  @tailrec //Choose first winner
  def playBingo1(boards: List[Board], drawNumbers: List[Int], drawPos: Int = 0): Int =
    if drawPos == drawNumbers.length then -1
    else {
      val winningBoard = boards.find(b => columnWon(b) || rowWon(b))
      if winningBoard.nonEmpty then calculateScore(winningBoard, drawNumbers(drawPos - 1))
      else
        playBingo1(
          boards.map(board =>
            board.copy(grid = board.grid.map(_.map(b => b.copy(marked = b.marked || b.value == drawNumbers(drawPos)))))
          ),
          drawNumbers,
          drawPos + 1
        )
    }

  @tailrec //Choose last winner
  def playBingo2(
      boards: List[Board],
      drawNumbers: List[Int],
      drawPos: Int = 0,
      winningOrder: List[Board] = List.empty
  ): Int =
    if drawPos == drawNumbers.length then calculateScore(winningOrder.lastOption, drawNumbers(drawPos - 1))
    else {
      val winningBoards  = boards.filter(b => columnWon(b) || rowWon(b))
      val updatedWinners = (winningOrder ++ winningBoards).distinctBy(_.id)
      if updatedWinners.size == boards.size then calculateScore(updatedWinners.lastOption, drawNumbers(drawPos - 1))
      else
        playBingo2(
          boards.map(board =>
            board.copy(grid = board.grid.map(_.map(b => b.copy(marked = b.marked || b.value == drawNumbers(drawPos)))))
          ),
          drawNumbers,
          drawPos + 1,
          updatedWinners
        )
    }

  case class Box(value: Int, marked: Boolean = false)

  case class Board(grid: List[List[Box]], id: Int)
}
