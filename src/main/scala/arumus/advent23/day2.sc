import scala.:+
import scala.annotation.tailrec
import scala.collection.immutable.Nil.tail
import scala.collection.mutable.Map
import scala.util.matching.Regex

val input =
  scala.io.Source.fromResource(s"advent23/day1.txt").getLines().toList

val numWords = List(
  "zero",
  "one",
  "two",
  "three",
  "four",
  "five",
  "six",
  "seven",
  "eight",
  "nine"
)

val numWordsWithIndex = numWords.zipWithIndex

def transformAsNumber(line: String) = {
  val numsOnly = line.filter(_.isDigit).map(_.asDigit)
  s"${numsOnly.headOption.getOrElse(0)}${numsOnly.lastOption.getOrElse(0)}".toInt
}

def transformWords(line: String) = {

  @tailrec
  def replaceAtStart(
    input: String,
    words: List[(String, Int)] = numWordsWithIndex
  ): String =
    words match {
      case Nil => input
      case (word, index) :: tail =>
        val result = input.replaceFirst(s"^${word}", index.toString)
        if (result == input) replaceAtStart(input, tail)
        else result
    }

  @tailrec
  def replaceWords(input: String,
                   index: Int,
                   inc: Int,
                   cond: Int => Boolean): String =
    if (!cond(index) || input.charAt(index).isDigit)
      input
    else {
      val suffix = input.substring(index)
      val replaced = replaceAtStart(suffix, numWordsWithIndex)
      if (replaced == suffix) replaceWords(input, index + inc, inc, cond)
      else input.substring(0, index) + replaced
    }

  val replacedFront = replaceWords(line, 0, 1, _ < line.length - 1)
  replaceWords(replacedFront, replacedFront.length - 1, -1, _ > 0)
}

//Exercise 1
input.map(transformAsNumber).sum

//Exercise 2
input.map(transformWords).map(transformAsNumber).sum
