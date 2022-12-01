package arumus.advent21

import java.lang.Math.abs

object Day7 {

  def calculateFuel(positions: List[Int]): Int = {
    val sorted = positions.sorted
    val median = sorted(positions.length / 2)
    positions.map(p => abs(median - p)).sum
  }

  def calculateFuel2(positions: List[Int]): Int =
    (positions.min to positions.max).map { targetPos =>
      positions.map { pos =>
        val diff = abs(targetPos - pos)
        diff * (diff + 1) / 2
      }.sum
    }.min
}
