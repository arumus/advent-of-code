import scala.:+
import scala.annotation.tailrec
import scala.util.matching.Regex

val input =
  scala.io.Source
    .fromResource(s"advent23/day2.txt")
    .getLines()
    .filter(_.nonEmpty)
    .toList

final case class ColorCount(blue: Int, green: Int, red: Int)

final case class Game(gameNo: Int, picks: List[ColorCount])

//Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
def mapGame(gameStr: String): Game = {

  def parseColor(colorStr: String): (String, Int) = colorStr.trim match {
    case s"$count $color" => (color, count.toInt)
  }

  def parseColors(colorsStr: String): Map[String, Int] =
    colorsStr.split(",").map(parseColor).toMap

  def parsePicks(picksStr: String): List[Map[String, Int]] =
    picksStr.split(";").map(parseColors).toList

  gameStr match {
    case s"Game $gameNo: $picksStr" =>
      Game(gameNo.toInt, parsePicks(picksStr).map { x =>
        ColorCount(
          blue = x.getOrElse("blue", 0),
          green = x.getOrElse("green", 0),
          red = x.getOrElse("red", 0)
        )
      })
  }
}

val games = input.map(mapGame).tapEach(println)
val bag = ColorCount(red = 12, green = 13, blue = 14)

val result1 = games.collect {
  case Game(id, picks)
      if picks.forall(
        pick =>
          pick.red <= bag.red && pick.green <= bag.green && pick.blue <= bag.blue
      ) =>
    id
}.sum

val result2 = games.map { game =>
  val picks = game.picks
  val red = picks.map(_.red).max
  val green = picks.map(_.green).max
  val blue = picks.map(_.blue).max
  red * green * blue
}.sum
