package arumus.advent21

import scala.annotation.tailrec

object Day2 {

  @tailrec
  def navigate1(plannedCourse: List[(String, Int)],
                depth: Int = 0,
                horizontal: Int = 0): (Int, Int) =
    plannedCourse match {
      case Nil                  => (depth, horizontal)
      case ("forward", v) :: xs => navigate1(xs, depth, horizontal + v)
      case ("down", v) :: xs    => navigate1(xs, depth + v, horizontal)
      case ("up", v) :: xs      => navigate1(xs, depth - v, horizontal)
      case _                    => throw new RuntimeException("bad...")
    }

  @tailrec
  def navigate2(plannedCourse: List[(String, Int)],
                depth: Int = 0,
                horizontal: Int = 0,
                aim: Int = 0): (Int, Int, Int) =
    plannedCourse match {
      case Nil => (depth, horizontal, aim)
      case ("forward", x) :: xs =>
        navigate2(xs, depth + (aim * x), horizontal + x, aim)
      case ("down", x) :: xs => navigate2(xs, depth, horizontal, aim + x)
      case ("up", x) :: xs   => navigate2(xs, depth, horizontal, aim - x)
      case _                 => throw new RuntimeException("bad...")

    }
}
