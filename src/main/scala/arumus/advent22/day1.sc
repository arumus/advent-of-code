import scala.collection.mutable.Map

val lines = scala.io.Source.fromResource(s"advent22/day1.txt").getLines().toList

val sortedGroups = lines
  .map(_.toIntOption.getOrElse(0))
  .foldLeft((0, 0, Map.empty[Int, Int]))((acc, x) => {
    val (group, currSum, map) = acc
    if (x == 0)
      (group + 1, 0, map += (group -> currSum))
    else
      (group, currSum + x, map)
  })._3.values.toList.sorted.reverse

val top = sortedGroups.head
val top3 = sortedGroups.take(3).sum
