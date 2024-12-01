val sections = scala.io.Source.fromResource(s"advent22/day4.txt").getLines().toList
val assignments = sections
  .map(_.split(","))
  .map(_.map(_.split("-").map(_.toInt)).map(x => Range.inclusive(x.head, x.last).toSet))
  .map(x => (x.head, x.last))

val round1=assignments.count(x => x._1.subsetOf(x._2) || x._2.subsetOf(x._1))

val round2=assignments.count(x => x._1.intersect(x._2).nonEmpty)


