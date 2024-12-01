val input = scala.io.Source.fromResource(s"advent22/day9.txt").getLines()

case class Move(direction: Char, steps: Int)

case class Coord(x: Int, y: Int)

val moves = input
  .map(_.split(" "))
  .map(x => (x.head.head, x.last.toInt))
  .flatMap(x => List.fill(x._2)(Move(x._1, 1)))
  .toList

def moveHead(move: Move, head: Coord): Coord =
  move.direction match {
    case 'R' => head.copy(x = head.x + 1)
    case 'U' => head.copy(y = head.y + 1)
    case 'L' => head.copy(x = head.x - 1)
    case 'D' => head.copy(y = head.y - 1)
  }

def catchupTail(head: Coord, tail: Coord): Coord = {
  val isTouchingHead = (head.x - tail.x).abs <= 1 && (head.y - tail.y).abs <= 1
  val is2AwayInXAxis = (head.x == tail.x && (head.y - tail.y).abs == 2)
  val is2AwayInYAxis = (head.y == tail.y && (head.x - tail.x).abs == 2)

  val (xInc, yInc) =
    if (isTouchingHead) (0, 0)
    else if (is2AwayInXAxis)
      if (head.y > tail.y) (0, 1) else (0, -1)
    else if (is2AwayInYAxis)
      if (head.x > tail.x) (1, 0) else (-1, 0)
    else
      if (head.x > tail.x && head.y > tail.y) (1, 1)
      else if (head.x < tail.x && head.y < tail.y) (-1, -1)
      else if (head.x > tail.x && head.y < tail.y) (1, -1)
      else if (head.x < tail.x && head.y > tail.y) (-1, 1)
      else (0, 0)

  tail.copy(x = tail.x + xInc, y = tail.y + yInc)
}

def simulate(ropeSize: Int, startingAt: Coord = Coord(0, 0)) = {
  moves.foldLeft((List.fill(ropeSize)(startingAt), List(startingAt))) {
    case ((rope, tailsPath), move) => {
      val updatedRope = rope.tail.foldLeft(List[Coord](moveHead(move, rope.head))) {
          case (accRope, knot) => accRope :+ catchupTail(accRope.last, knot)
        }
      (updatedRope, updatedRope.last :: tailsPath)
    }
  }
}

val round1 = simulate(2)._2.distinct.size
val round2 = simulate(10)._2.distinct.size



