val input = scala.io.Source.fromResource(s"advent22/day2.txt").getLines().toList

val strategy = input.map(x => x.split(" ")).map(x => (x.head, x.last))
val score = Map("X" -> 1, "Y" -> 2, "Z" -> 3)
val winScore = 6
val looseScore = 0
val drawScore = 3

def result1( oppChoice: String, yourChoice:String): Int =  (oppChoice, yourChoice) match {
  case ("A", "Y") => winScore
  case ("B", "Z") => winScore
  case ("C", "X") => winScore
  case ("A", "X") => drawScore
  case ("B", "Y") => drawScore
  case ("C", "Z") => drawScore
  case _ => looseScore
}
val rount1 = strategy.foldLeft(0)((acc, x) => {
  val (oppChoice, yourChoice) = x
  acc + score(yourChoice) + result1(oppChoice, yourChoice)
})

val winingMove = Map("A" -> "Y", "B" -> "Z", "C" -> "X")
val drawMove = Map("A" -> "X", "B" -> "Y", "C" -> "Z")
val loosingMove = Map("A" -> "Z", "B" -> "X", "C" -> "Y")

def result2(oppChoice:String, move:String): Int =  (move) match {
  case ("X") => looseScore + score(loosingMove(oppChoice))
  case ("Y") => drawScore + score(drawMove(oppChoice))
  case ("Z") => winScore + score(winingMove(oppChoice))
  case _ => 0
}

val rount2 = strategy.foldLeft(0)((acc, x) => {
  val (oppChoice, move) = x
  acc + result2(oppChoice, move)
})



