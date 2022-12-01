package arumus.advent21

import java.lang.Integer.parseInt
import java.lang.Math.round
import scala.annotation.tailrec

object Day3 {

  @tailrec
  def calculateRates(
      readings: List[String],
      gammaRate: String = "",
      epsilonRate: String = "",
      pos: Int = 0
  ): (String, String, Int) =
    if (pos == readings.head.length) (gammaRate, epsilonRate, pos)
    else if (readings.count(_.charAt(pos) == '1') > round(readings.length / 2.0)) //cheated here by using map and sum.
      calculateRates(readings, gammaRate + "1", epsilonRate + "0", pos + 1)
    else
      calculateRates(readings, gammaRate + "0", epsilonRate + "1", pos + 1)

  def calculateLifeSupport(readings: List[String]): Int = {
    @tailrec
    def findRatings(readings: List[String], bitPref: (Int, Int), pos: Int = 0): List[String] =
      if (readings.length <= 1 || pos == readings.head.length) readings
      else
        findRatings(
          readings.filter {
            _.charAt(pos) == (if (readings.count(_.charAt(pos) >= '1') >= round(readings.length / 2.0)) bitPref._1
                              else bitPref._2)
          },
          bitPref,
          pos + 1
        )

    val oxygenRating   = findRatings(readings, ('1', '0')).headOption.getOrElse("1")
    val scrubberRating = findRatings(readings, ('0', '1')).headOption.getOrElse("1")
    parseInt(oxygenRating, 2) * parseInt(scrubberRating, 2)
  }

}
