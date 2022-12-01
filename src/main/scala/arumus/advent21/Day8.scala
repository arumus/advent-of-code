package arumus.advent21

object Day8 {
  /*
    0 => 6 = abcefg
    1 => 2 = cf *
    2 => 5 = acdeg
    3 => 5 = acdfg
    4 => 4 = bdcf *
    5 => 5 = abdfg
    6 => 6 = abdefg
    7 => 3 = acf *
    8 => 7 = abcdefg *
    9 => 6 = abcdfg
   */
  def processSignal2(inputSignals: List[InputSignal]): Int = {
   ???
  }

  def processSignal(inputSignals: List[InputSignal]): Int =
    inputSignals.map { inputSignal =>
      val patterns = mapSegments(inputSignal.signals)
      inputSignal.numbers.map(_.sorted).count(patterns(_).nonEmpty)
    }.sum

  def mapSegments(signals: List[String]): Map[String, Option[Int]] =
    signals
      .map(_.sorted)
      .map(x =>
        if (x.length == 2) (x, Some(1))
        else if (x.length == 4) (x, Some(4))
        else if (x.length == 3) (x, Some(7))
        else if (x.length == 7) (x, Some(8))
        else (x, None)
      )
      .toMap

  case class InputSignal(signals: List[String], numbers: List[String])
}
