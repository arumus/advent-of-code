import scala.collection.{SortedSet, mutable}

val input = scala.io.Source.fromResource(s"advent22/day7.txt").getLines()

val (dirSizes, _) = input.foldLeft(
  mutable.SortedMap.empty[String, Long],
  mutable.Stack.empty[String]
) {
  case ((entries, currPath), entry) =>
    entry match {
      case s"$$ cd .." => currPath.pop; (entries, currPath)
      case s"$$ cd /"  => currPath.push("/"); (entries, currPath)
      case s"$$ cd $dir" => currPath.push(currPath.top + dir + "/"); (entries, currPath)
      case s"$$ ls"    => (entries, currPath)
      case s"dir $dir" => (entries, currPath)
      case s"$size $file" => (currPath.foldLeft(entries) {
        case (allEntries, path) =>  allEntries += path -> ((entries.getOrElse(path, 0L) + size.toLong))
      }, currPath)
    }
}

dirSizes.foreach(x => println(x._1 + "=> " + x._2))

val round1=dirSizes.collect {
  case (x, size) if size < 100_000L => size
}.sum

val diff = dirSizes("/") - 40_000_000L

val round2=dirSizes.collect {
  case (x, size) if size >= diff => size
}.min
