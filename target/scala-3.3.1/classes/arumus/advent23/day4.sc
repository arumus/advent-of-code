import javax.smartcardio.Card
import scala.Console.in
import scala.annotation.tailrec
import scala.util.matching.Regex
import scala.collection.mutable

val input = scala.io.Source
    .fromResource(s"advent23/day4.txt")
    .getLines()
    .filter(_.nonEmpty)
    .toList

def parseNumbers(str:String): Set[Int] = str.split(" ")
  .map(_.trim)
  .filter(_.nonEmpty)
  .map(_.toInt)
  .toSet

final case class Card(id: Int, winningNumbers:Set[Int], cardNumbers:Set[Int]) {
  def cardWinners = winningNumbers.intersect(cardNumbers)

  def winningCopies(allCards: List[Card]): List[Card] =
    (id until id + cardWinners.size).filter(_ < allCards.size).map(allCards.apply).toList
}

def parseCard(cardStr: String): Card =  cardStr match
  case s"Card $id: $rest" =>
    val numbers = rest.split("\\|")
    val winningNumbers = parseNumbers(numbers.head)
    val cardNumbers = parseNumbers(numbers.last)
    Card(id.trim.toInt, winningNumbers, cardNumbers)

val allCards = input.map(parseCard)

def updateCardTracker(cards: List[Card]): Int = {
  val queue = mutable.Queue.from(cards)
  val tracker = Array.fill(cards.size)(0)
  while (queue.nonEmpty) do {
    val card = queue.dequeue()
    tracker(card.id - 1) += 1
    queue.enqueueAll(card.winningCopies(cards))
  }
  tracker.sum
}

val result1 = allCards
  .map{ card => Math.pow(2, card.cardWinners.size -1).toInt
  }.sum

val result2 = updateCardTracker(allCards)
