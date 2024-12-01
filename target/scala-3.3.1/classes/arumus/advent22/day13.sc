import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

val input =
  scala.io.Source.fromResource(s"advent22/day13.txt").getLines().toList

@tailrec
def parse(
  signal: List[Char],
  stack: mutable.Stack[List[Any]] = mutable.Stack.empty
): List[Any] = {
  signal match {
    case '[' :: xs =>
      stack.push(List.empty)
      parse(xs, stack)
    case ']' :: Nil =>
      stack.pop
    case ']' :: xs =>
      val popped = stack.pop()
      stack.update(0, stack.top :+ popped)
      parse(xs, stack)
    case ',' :: xs =>
      parse(xs, stack)
    case _ =>
      val (num, xs) = signal.span(_.isDigit)
      stack.update(0, stack.top :+ num.mkString.toInt)
      parse(xs, stack)
  }
}

val refinedInput = input
  .filter(_.nonEmpty)
  .grouped(2)
  .map(x => (parse(x.head.toList), parse(x.last.toList)))
  .toList

def compare(left: List[Any], right: List[Any]): Int = {
  if (left.isEmpty && right.isEmpty) 0
  else if (left.isEmpty && right.nonEmpty) -1
  else if (left.nonEmpty && right.isEmpty) 1
  else {
    val result = (left.head, right.head) match {
      case (l: Int, r: Int)             => if (l < r) -1 else if (l > r) 1 else 0
      case (l: Int, r: List[Any])       => compare(List(l), r)
      case (l: List[Any], r: Int)       => compare(l, List(r))
      case (l: List[Any], r: List[Any]) => compare(l, r)
    }
    if (result == 0) compare(left.tail, right.tail) else result
  }
}

val round1 = refinedInput.zipWithIndex.collect {
  case ((left, right), index) if compare(left, right) < 0 => {
    index + 1L
  }
}.sum

val sortedList=(input.filter(_.nonEmpty) ++ List("[[2]]", "[[6]]")).map(_.toList)
  .map(x => parse(x))
  .sortWith((x, y) => compare(x, y) <=0)

val round2 = (sortedList.indexWhere(_ ==List(List(2))) + 1 ) *
             (sortedList.indexWhere(_ == List(List(6))) + 1)
