import com.sun.tools.attach.VirtualMachine.list

import scala.:+
import scala.collection.mutable.Map

val rucksacks = scala.io.Source.fromResource(s"advent22/day3.txt").getLines().toList

def sharedChar(comp: List[Set[Char]]): Char = {
  comp.foldLeft(Set.empty[Char]) {
      case (acc, curr) if (acc.nonEmpty) => acc.intersect(curr)
      case (_, curr) => curr
    }.head
}
def priority(c: Char): Int = if (c.isLower) (c - 'a' + 1) else (c - 'A' + 27)

// 7878
val round1 = rucksacks
  .map(x => x.splitAt(x.length / 2))
  .map(x => sharedChar(List(x._1.toCharArray.toSet, x._2.toCharArray.toSet)))
  .map(priority).sum

val round2 = rucksacks.foldLeft((1, List.empty[String], 0)) {
  case ((index, group, acc), x) if (group.nonEmpty && index % 3 == 0) =>  (index + 1, List.empty, acc + priority(sharedChar((group :+ x).map(_.toCharArray.toSet))))
  case ((index, group, acc), x) => (index + 1, group :+ x, acc)
}._3

