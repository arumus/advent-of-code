import scala.:+
import scala.Console.in
import scala.annotation.tailrec
import scala.util.matching.Regex

val input =
  scala.io.Source
    .fromResource(s"advent23/day3.txt")
    .getLines()
    .filter(_.nonEmpty)
    .toList

final case class Coord(row: Int, col: Int)

//Parsed number and its starting index (col)
def parseNumbers(row: Int, rowData: String): List[(Int, Coord)] =
  "(\\d+)".r //numberPattern
    .findAllMatchIn(rowData)
    .map(x => (x.group(1).toInt, Coord(row, x.start)))
    .toList

def hasSpecialChar(at: Coord): Boolean =
  !input(at.row).charAt(at.col).isDigit && input(at.row).charAt(at.col) != '.'

def hasStar(at: Coord): Boolean =
  input(at.row).charAt(at.col) == '*'

//Find coordinates of all special chars adjacent to a given coordinate
def neighbours(at: Coord, charMatch: Coord => Boolean): List[Coord] = {
  for {
    rowIncrement <- List(-1, 0, 1)
    colIncrement <- List(-1, 0, 1)
    if !(rowIncrement == 0 && colIncrement == 0)
  } yield Coord(at.row + rowIncrement, at.col + colIncrement)
}.filter {
    case Coord(row, col) =>
      row >= 0 && row < input.length &&
        col >= 0 && col < input(row).length
  }
  .filter(charMatch)

def neighboursForNum(num: Int, at: Coord, charMatch: Coord => Boolean) =
  (for {
    c <- at.col until at.col + num.toString.length
    matchedCoord <- neighbours(Coord(at.row, c), charMatch)
  } yield (matchedCoord -> num)).distinct

val result1 = (for {
  row <- input.indices
  dataRow = input(row)
  numbersInRow = parseNumbers(row, dataRow)
  rowSum = numbersInRow
    .filter((num, at) => neighboursForNum(num, at, hasSpecialChar).nonEmpty)
    .map(_._1)
    .sum
} yield rowSum).sum

val result2 = (for {
  row <- input.indices
  dataRow = input(row)
  numbersInRow = parseNumbers(row, dataRow)
  gearWithNumbers <- numbersInRow.flatMap(
    (num, at) => neighboursForNum(num, at, hasStar)
  )
} yield gearWithNumbers)
  .groupBy { case (coord, _) => coord }
  .collect { case (_, pairs) if pairs.length == 2 => pairs.map(_._2).product }
  .sum


