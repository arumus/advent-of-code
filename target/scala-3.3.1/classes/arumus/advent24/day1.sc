import scala.:+
import scala.annotation.tailrec
import scala.collection.immutable.Nil.tail
import scala.collection.mutable.Map
import scala.math.abs
import scala.util.matching.Regex

val input =
  scala.io.Source.fromResource(s"advent24/day1.example.txt").getLines().toList

val pattern = """(\d+)\s+(\d+)""".r

val (group1Result, group2Result) = input.map { case pattern(a, b) =>
  (a.toInt, b.toInt)
}.unzip

val result1 = (group1Result.sorted zip group2Result.sorted).map { case (a, b) =>
  abs(a - b)
}.sum

val frequency = group2Result.groupBy(identity).view.mapValues(_.size).toMap

val result2 = group1Result.map { a =>
  a * frequency.getOrElse(a, 0)
}.sum
