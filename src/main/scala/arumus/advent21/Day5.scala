package arumus.advent21

object Day5 {

  def identifyVentOverlaps(vents: Seq[Vent]): Int =
    vents
      .filter(v => v.from.x == v.to.x || v.from.y == v.to.y)
      .flatMap(_.flattenVentPoints)
      .groupBy(identity)
      .count(x => x._2.size > 1)

  def identifyVentOverlaps2(vents: Seq[Vent]): Int =
    vents
      .flatMap(_.flattenVentPoints)
      .groupBy(identity)
      .count(x => x._2.size > 1)

  case class Point(x: Int, y: Int)
  case class Vent(from: Point, to: Point) {
    def flattenVentPoints: Seq[Point] = {
      val xRange = if from.x <= to.x then from.x to to.x else from.x to to.x by -1
      val yRange = if from.y <= to.y then from.y to to.y else from.y to to.y by -1
      xRange.zipAll(yRange, from.x, to.y).map(p => Point(p._1, p._2))
    }
  }

}
