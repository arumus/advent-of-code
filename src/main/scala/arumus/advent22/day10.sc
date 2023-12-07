val input =
  scala.io.Source.fromResource(s"advent22/day10.txt").getLines().toList

val registerXValues =
  input.foldLeft(List(1)) {
    case (signals, s"addx ${value}") =>
      signals ++ List(signals.last) :+ (signals.last + value.toInt)
    case (signals, s"noop") => signals ++ List(signals.last)
  }

val rowSize = 40
val initialCycle = 20
val round1=(60 to 220 by rowSize)
  .foldLeft(registerXValues(initialCycle -1) * initialCycle) {
    case (acc, ithCycle) =>  acc + registerXValues(ithCycle - 1) * ithCycle
  }

(0 until 240).foreach { cycle =>
  val crtPos = (cycle % rowSize)
  val sprite = registerXValues(cycle)
  val pixel = if ((crtPos - sprite).abs <= 1) "#" else "."
  val printablePixel = if (cycle % rowSize == (rowSize - 1)) pixel + "\n" else pixel
  print(printablePixel)
}
