import scala.collection.mutable.Map

val input = scala.io.Source.fromResource(s"advent22/day1.txt").getLines().toList

val sortedGroups = input
  .map(_.toIntOption.getOrElse(0))
  .foldLeft((0, 0, Map.empty[Int, Int]))((acc, x) => {
    val (group, currSum, map) = acc
    if (x == 0)
      (group + 1, 0, map += (group -> currSum))
    else
      (group, currSum + x, map)
  })._3.values.toList.sorted.reverse

val rount1 = sortedGroups.head
val round2 = sortedGroups.take(3).sum
