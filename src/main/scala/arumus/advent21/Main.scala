package arumus.advent21

import arumus.advent21.Day1.*
import arumus.advent21.Day2.*
import arumus.advent21.Day3.*
import arumus.advent21.Day4.*
import arumus.advent21.Day5.*
import arumus.advent21.Day6.*
import arumus.advent21.Day7.*

import scala.annotation.tailrec
import scala.io.Source

object Main extends App {

  //Verify results
  verifyDay1()
  verifyDay2()
  verifyDay3()
  verifyDay4()
  verifyDay5()
  verifyDay6()
  verifyDay7()

  def verifyDay1(): Unit = {
    val inputArr = readInput("advent21/day1.txt").map(_.toInt)

    val resultForWindowBy2Lang = Day1.countIncreasesByWindow2(inputArr)
    val resultForWindowBy2Basic = Day1Basic.countIncreasesByWindow2(inputArr)
    println(s"day1: prob1 result=$resultForWindowBy2Lang")
    assert(1462 == resultForWindowBy2Lang)
    assert(resultForWindowBy2Lang == resultForWindowBy2Basic)

    val resultForWindowBy3Lang = Day1.countIncreasesByWindow3(inputArr)
    val resultForWindowBy3Basic = Day1Basic.countIncreasesByWindow3(inputArr)
    println(s"day1: prob2 result=$resultForWindowBy3Lang")
    assert(1497 == resultForWindowBy3Lang)
    assert(resultForWindowBy3Lang == resultForWindowBy3Basic)
    println("day1: success")
  }

  def verifyDay2(): Unit = {
    val inputArr = readInput("advent21/day2.txt")
      .map(_.split(" "))
      .map(x => (x.head, x.last.toInt))
      .toList

    val (depth, horizontal) = Day2.navigate1(inputArr)
    val multipliedResult1 = depth * horizontal
    println(s"day2: prob1 result=$multipliedResult1")
    assert(1728414 == multipliedResult1)

    val (depth2, horizontal2, _) = Day2.navigate2(inputArr)
    val multipliedResult2 = depth2 * horizontal2
    println(s"day2: prob2 result=$multipliedResult2")
    assert(1765720035 == multipliedResult2)

    println("day2: success")
  }

  def verifyDay3(): Unit = {
    val inputArr = readInput("advent21/day3.txt").toList

    val (gamma, epsilon, _) = Day3.calculateRates(inputArr)
    val result1 = Integer.parseInt(gamma, 2) * Integer.parseInt(epsilon, 2)
    println(s"day3: prob1 result=$result1")
    assert(2250414 == result1)

    val result2 = Day3.calculateLifeSupport(inputArr)
    println(s"day3: prob2 result=$result2")
    assert(6085575 == result2)

    println("day3: success")
  }

  def verifyDay4(): Unit = {
    val inputArr = readInput("advent21/day4.txt").toList

    def readBoard(inputArr: List[String], id: Int): Day4.Board =
      Day4.Board(
        for {
          strLine <- inputArr
            .map(_.trim)
            .dropWhile(_.isEmpty)
            .takeWhile(_.nonEmpty)
          box = strLine
            .split(" ")
            .filter(_.nonEmpty)
            .map(_.trim().toInt)
            .map(Day4.Box(_))
            .toList
        } yield box,
        id
      )

    @tailrec
    def readBoards(inputArr: List[String],
                   acc: List[Day4.Board] = List.empty,
                   id: Int = 0): List[Day4.Board] =
      if (inputArr.isEmpty) acc
      else
        readBoards(
          inputArr.map(_.trim).dropWhile(_.isEmpty).dropWhile(_.nonEmpty),
          acc :+ readBoard(inputArr, id),
          id + 1
        )

    def printBoard(board: Day4.Board): Unit = {
      println
      board.grid.foreach { r =>
        r.foreach { x =>
          val v = x.value
          val m = if (x.marked) "✓" else "."
          print(f"$v%2d$m%s ")
        }
        println
      }
    }

    val drawNumbers = inputArr.head.split(",").map(_.toInt).toList
    val boards = readBoards(inputArr.tail)
    val result1 = Day4.playBingo1(boards, drawNumbers)
    val result2 = Day4.playBingo2(boards, drawNumbers)

    println(s"day4: prob1 result=$result1")
    println(s"day4: prob2 result=$result2")
    //assert(4512 == result1)
    //assert(1924 == result2)
    assert(5685 == result1)
    assert(21070 == result2)

    println("day4: success")

  }

  def verifyDay5(): Unit = {
    val inputArr = readInput("advent21/day5.txt").toList
    val vents: Seq[Vent] = inputArr.map(r => r.split(" -> ")).map { ventStr =>
      val linePoints = ventStr.map { pointStr =>
        val pointArr = pointStr.split(",")
        Point(pointArr.head.toInt, pointArr.last.toInt)
      }
      Vent(linePoints.head, linePoints.last)
    }

    val result1 = Day5.identifyVentOverlaps(vents)
    val result2 = Day5.identifyVentOverlaps2(vents)

    println(s"day5: prob1 result=$result1")
    println(s"day5: prob2 result=$result2")

    //assert(5 == result1)
    //assert(12 == result2)

    assert(7436 == result1)
    assert(21104 == result2)
    println("day5: success")

  }

  def readInput(inputFile: String): Array[String] =
    Source.fromResource(inputFile).getLines.toArray

  def verifyDay6(): Unit = {
    val inputArr = readInput("advent21/day6.txt").head.split(",").map(_.toInt)
    val result1 = Day6.simulateLanternFish(inputArr, 80)
    println(s"day6: prob1 result=$result1")
    val result2 = Day6.simulateLanternFish(inputArr, 256)
    println(s"day6: prob2 result=$result2")
    //assert(5 == result1)
    //assert(12 == result2)
    assert(380612 == result1)
    assert(1710166656900L == result2)
    println("day6: success")
  }

  def verifyDay7(): Unit = {
    val inputArr =
      readInput("advent21/day7.txt").head.split(",").map(_.toInt).toList
    val result1 = Day7.calculateFuel(inputArr)
    println(s"day7: prob1 result=$result1")
    assert(340056 == result1)
    val result2 = Day7.calculateFuel2(inputArr)
    println(s"day7: prob2 result=$result2")
    assert(96592275 == result2)
    println("day7: success")
  }
//
//  def verifyDay8(): Unit = {
//    val inputArr = readInput("advent21/day8.txt")
//    val input = inputArr
//      .map(line => line.split('|'))
//      .map(
//        lineParts =>
//          InputSignal(
//            lineParts.head.split(" ").toList,
//            lineParts.last.split(" ").filter(_.nonEmpty).toList
//        )
//      )
//      .toList
//
//    val result1 = Day8.processSignal(input)
//    println(s"day8: prob1 result=$result1")
//    assert(237 == result1)
//    println("day8: success")
//  }

}
