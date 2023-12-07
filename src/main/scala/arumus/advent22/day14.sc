
val input = scala.io.Source.fromResource(s"advent22/temp.txt").getLines().toList

case class Coord(row:Int, col:Int)
val parsedInput = input.map(_.split("->").map(_.split(",").map(_.trim.toInt)).map(x => Coord(x.last, x.head)))

val rowStart =  0
val rowEnd =  parsedInput.flatMap(_.map(_.row)).max
val colStart =  parsedInput.flatMap(_.map(_.col)).min
val colEnd =  parsedInput.flatMap(_.map(_.col)).max
val sandSource = Coord(0, 500)

