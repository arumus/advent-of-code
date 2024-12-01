import scala.collection.immutable.{ListMap, SortedMap}
import scala.collection.mutable

val input =
  scala.io.Source.fromResource(s"advent22/day6.txt").getLines().next()

def findStart(windowSize: Int): Int =
  input.toCharArray.sliding(windowSize).takeWhile(_.toSet.size < windowSize).size + windowSize

val round1=findStart(4)
val round2=findStart(14)

