package arumus.advent21

import scala.annotation.tailrec

object Day1 {
  def countIncreasesByWindow2(readings: Array[Int]): Int =
    readings.sliding(2).count(w => w.head < w.last)

  def countIncreasesByWindow3(readings: Array[Int]): Int =
    readings.sliding(3).map(_.sum).sliding(2).count(w => w.head < w.last)
}

object Day1Basic {

  @tailrec
  def windowSum(readings: Array[Int], start: Int, windowSize: Int = 1, curr: Int = 0, acc: Int = 0): Int =
    if (start < 0 || (start + curr) >= readings.length || curr == windowSize) acc
    else
      windowSum(
        readings,
        start,
        windowSize,
        curr + 1,
        acc + readings(start + curr)
      )

  @tailrec
  def countIncreasesByWindow2(readings: Array[Int], curr: Int = 1, acc: Int = 0): Int =
    if (curr == readings.length) acc
    else
      countIncreasesByWindow2(
        readings,
        curr + 1,
        if (windowSum(readings, curr) > windowSum(readings, curr - 1)) acc + 1 else acc
      )

  @tailrec
  def countIncreasesByWindow3(readings: Array[Int], curr: Int = 1, acc: Int = 0): Int =
    if (curr == readings.length) acc
    else
      countIncreasesByWindow3(
        readings,
        curr + 1,
        if (windowSum(readings, curr, 3) > windowSum(readings, curr - 1, 3)) acc + 1 else acc
      )

}
