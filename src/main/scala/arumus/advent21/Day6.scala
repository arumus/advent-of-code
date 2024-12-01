package arumus.advent21

import scala.annotation.tailrec

object Day6 {

  def simulateLanternFish(fishAges: Array[Int], numberOfDays: Int): Long = {

    @tailrec
    def loop(fishAgesGrouped: Seq[Long], currDay: Int): Long =
      if currDay == numberOfDays then fishAgesGrouped.sum
      else
        loop(
          fishAgesGrouped.indices.foldLeft(Seq.fill(9)(0L)) { (ageGroup, age) =>
            if age == 0 then ageGroup.updated(6, fishAgesGrouped(age)).updated(8, fishAgesGrouped(age))
            else ageGroup.updated(age - 1, ageGroup(age - 1) + fishAgesGrouped(age))
          },
          currDay + 1
        )

    loop(
      fishAges.foldLeft(Seq.fill(9)(0L)) { (ageGroup, age) =>
        ageGroup.updated(age, ageGroup(age) + 1)
      },
      0
    )

    /*
    3,4,3,1,2
    0,0,0,0,0
    0.0,0.1,0
    0.0,0.1,1
    0.1,1.2,1

     */
  }

}
