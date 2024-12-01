import scala.collection.mutable
import scala.compiletime.ops.int

val input =
  scala.io.Source.fromResource(s"advent22/day11.txt").getLines().toList

final case class Monkey(index: Int, items: List[Long], worryLevel: Long => Long, test: Long => Int, divisibleBy:Int, inspectedTimes: Long = 0)

def extractMonkeyNo(str: String) =
    str.trim match {case s"Monkey $monkeyNo:" => monkeyNo.toInt}

def extractItems(str: String): List[Long] =
  str.trim match {case s"Starting items: $items" => items.split(", ").map(_.trim.toLong).toList}

def extractOperation(str: String): (Long => Long) =
  old => str.trim match {
      case s"Operation: new = old * old" => old * old
      case s"Operation: new = old * $var1" => old * var1.trim.toInt
      case s"Operation: new = old + old" => old + old
      case s"Operation: new = old + $var1" => old + var1.trim.toInt
  }

def extractDivisibleBy(str: String):Int =
  str.trim match {case s"Test: divisible by $d" => d.toInt}

def extractTest(strArr: Seq[String]): (Long => Int) =
  old => {
    val divisibleBy = extractDivisibleBy(strArr.head)
    val trueMonkey = strArr(1).trim match {case s"If true: throw to monkey $x" => x.trim.toInt}
    val falseMoney = strArr(2).trim match {case s"If false: throw to monkey $x" => x.trim.toInt}
    if (old % divisibleBy == 0) trueMonkey else falseMoney
  }

val monkeys = input.iterator
  .filter(_.nonEmpty)
  .grouped(6)
  .map { strArray =>
    Monkey(
      extractMonkeyNo(strArray.head),
      extractItems(strArray(1)),
      extractOperation(strArray(2)),
      extractTest(strArray.takeRight(3)),
      extractDivisibleBy(strArray(3))
    )
  }.toList

monkeys.foreach(println)

def simulateMonkeys(monkeys: Array[Monkey], reduceFunc: Long => Long) =
  monkeys.foreach { monkey =>
    val items = monkey.items
    monkeys(monkey.index) = monkey.copy(
      items = List.empty,
      inspectedTimes = monkey.inspectedTimes + items.size
    )
    items.foreach { item =>
      val reducedWorryLevel = reduceFunc(monkey.worryLevel(item))
      val tossToMonkeyId = monkey.test(reducedWorryLevel)
      val tossToMonkey = monkeys(tossToMonkeyId)
      monkeys(tossToMonkeyId) = tossToMonkey.copy(items = tossToMonkey.items :+ reducedWorryLevel)
    }
  }


def runRound1 =
  val monkeysArr = monkeys.toArray
  (1 to 20).foreach { _ =>
    simulateMonkeys(monkeysArr, _ / 3L)
  }
  monkeysArr.foreach(println)
  monkeysArr
    .map(_.inspectedTimes)
    .sorted
    .takeRight(2)
    .product

runRound1

def runRound2 =
  val monkeysArr = monkeys.toArray
  val lcm= monkeys.map(_.divisibleBy).product
  (1 to 10000).foreach { _ =>
    simulateMonkeys(monkeysArr, _ % lcm)
  }
  monkeysArr.foreach(println)

  monkeysArr
    .map(_.inspectedTimes)
    .sorted
    .takeRight(2)
    .product

runRound2
