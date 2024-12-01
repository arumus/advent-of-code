val input = scala.io.Source.fromResource(s"advent22/day8.txt").getLines()
val grid = input.map(_.toCharArray.map(_ - '0').toList).toList

val totalRows = grid.size
val totalColumns = grid.head.size

val allCoords = for {
  row <- grid.indices
  col <- grid.head.indices
} yield (row, col)

def isTreeVisible(row: Int, col: Int): Boolean = {
  (0 until col).forall(c => grid(row)(c) < grid(row)(col)) || //Left
    (col + 1 until totalColumns)
      .forall(c => grid(row)(c) < grid(row)(col)) || //Right
    (0 until row).forall(r => grid(r)(col) < grid(row)(col)) || //Top
    (row + 1 until totalRows)
      .forall(r => grid(r)(col) < grid(row)(col)) //Bottom
}

val round1 = allCoords.count(isTreeVisible)
def scenicScore(row: Int, col: Int): Int = {
  val left = (col - 1 until 0 by -1).takeWhile(c => grid(row)(c) < grid(row)(col)).size + 1
  val right = (col + 1 until totalColumns-1).takeWhile(c => grid(row)(c) < grid(row)(col)).size + 1
  val up = (row - 1 until 0 by -1).takeWhile(r => grid(r)(col) < grid(row)(col)).size + 1
  val down = (row + 1 until totalRows-1).takeWhile(r => grid(r)(col) < grid(row)(col)).size + 1
  left * right * up * down
}

val round2 = allCoords.map(scenicScore).max
