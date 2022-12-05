import scala.:+
import scala.collection.mutable.Map

val input = scala.io.Source.fromResource(s"advent22/day1.txt").getLines().toList

val caloriesWithElves = input
  .map(_.toIntOption.getOrElse(0))
  .foldLeft((0, List.empty[Int])) {
    case ((currSum, list), x) if (x == 0)  => (0, list :+ currSum)
    case ((currSum, list), x) => (currSum + x, list)
  }._2.sorted.reverse

val round1 = caloriesWithElves.head

val round2 = caloriesWithElves.take(3).sum
