val input =
  scala.io.Source.fromResource(s"advent22/day2.txt").getLines().toList

val strategy = input.map(x => x.split(" ")).map(x => (x.head, x.last))

val winScore = 6
val looseScore = 0
val drawScore = 3
val score = Map("X" -> 1, "Y" -> 2, "Z" -> 3)

def mapScore (app: String, my:String):Int = (app, my) match {
  case ("A", "Y") => winScore + score(my)
  case ("B", "Z") => winScore + score(my)
  case ("C", "X") => winScore + score(my)
  case ("A", "X") => drawScore + score(my)
  case ("B", "Y") => drawScore + score(my)
  case ("C", "Z") => drawScore + score(my)
  case ("A", "Z") => looseScore + score(my)
  case ("B", "X") => looseScore + score(my)
  case ("C", "Y") => looseScore + score(my)
}

val translate =
  Map(
    "X" -> Map("A" -> "Z", "B" -> "X", "C" -> "Y"), // Loose
    "Y" -> Map("A" -> "X", "B" -> "Y", "C" -> "Z"), // Win
    "Z" -> Map("A" -> "Y", "B" -> "Z", "C" -> "X") // Draw
  )

val round1 = strategy.collect(mapScore).sum

val round2 = strategy
  .map { (opp, action) =>(opp, translate(action)(opp))}
  .map(mapScore)
  .sum
