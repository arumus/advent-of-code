import scala.collection.immutable.{ListMap, SortedMap}
import scala.collection.mutable

val allInput =
  scala.io.Source.fromResource(s"advent22/day5.txt").getLines().toList

val cratesList = allInput
  .takeWhile(_.contains("["))
  .map(x => x.sliding(4, 4).map(_.charAt(1)).toList)

def initializeStacks =
  SortedMap.from {
    for {
      c <- cratesList.head.indices
      stack = mutable.Stack[Char]()
      _ = for {
        r <- cratesList.indices.reverse
        v = cratesList(r)(c)
        _ = if (v.isLetter) stack.push(cratesList(r)(c))
      } yield ()
    } yield (c + 1).toString -> stack
  }

def parseMoves = {
  val movePattern = "move (\\d*) from (\\d?) to (\\d?)".r
  allInput.filter(_.startsWith("move")).map {
    case movePattern(numCrates, fromStack, toStack) =>
      ((numCrates.toInt), fromStack, toStack)
  }
}

val moves = parseMoves

//Round 1
val round1Stacks = initializeStacks
moves.foreach {
  case (numCrates, fromStack, toStack) =>
    (1 to numCrates).foreach(
      _ => round1Stacks(toStack).push(round1Stacks(fromStack).pop)
    )
}
val round1 = round1Stacks.values.collect(_.top).mkString

//Round 2
val round2Stacks = initializeStacks
moves.foreach {
  case (numCrates, fromStack, toStack) => {
    (1 to numCrates)
      .map(_ => round2Stacks(fromStack).pop)
      .reverse
      .foreach(x => round2Stacks(toStack).push(x))
  }
}
val round2 = round2Stacks.values.collect(_.top).mkString
