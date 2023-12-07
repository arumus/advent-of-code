import scala.annotation.tailrec
import scala.collection.mutable

val input = scala.io.Source.fromResource(s"advent22/day12.txt").getLines().toList
val heightGrid = input.map(_.toCharArray).map(_.toList)

val rowsSize = heightGrid.size
val colsSize =  heightGrid.head.size

val startWith = 'S'
val endWith = 'E'

case class Coord(row: Int, col: Int) {
  def originalHeight: Char =
    heightGrid(row)(col)

  def height: Char =
    heightGrid(row)(col) match
      case `startWith` => 'a'
      case `endWith` => 'z'
      case height => height

  def neighbours: List[Coord] =
     List(this.copy(col = col - 1), this.copy(col = col + 1), this.copy(row = row - 1), this.copy(row = row + 1))
      .filter(x => x.col >= 0 && x.row >= 0 && x.col < colsSize && x.row < rowsSize)
      .filter(x => (x.height - this.height) <= 1)
}

@tailrec
def shortestPath(pending: List[(Coord, Int)], visited: Set[Coord] = Set.empty): Int = {
  pending match
    case Nil =>
      Int.MaxValue
    case (curr, distance) :: _ if curr.originalHeight == endWith =>
      distance
    case (curr, _) :: remaining if visited.contains(curr) =>
      shortestPath(remaining, visited)
    case (curr, distance) :: remaining =>
      shortestPath(remaining ++ curr.neighbours.filterNot(visited.contains).map(x => (x, 1 + distance)), visited + curr)
}

def shortestPathFor(start:Char = startWith) =
  for {
    row <- 0 until rowsSize
    col <- 0 until colsSize
    if heightGrid(row)(col) == start
  } yield shortestPath(List((Coord(row, col), 0)))

shortestPathFor('S').min

shortestPathFor('a').min
