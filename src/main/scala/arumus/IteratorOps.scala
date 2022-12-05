package arumus

object IteratorOps {
  implicit class GroupIterator[A](x: Iterator[A]) {
    def groupedBy(groupBy: A => Boolean): Iterator[List[A]] =
      new Iterator[List[A]] {
        override def hasNext: Boolean = x.hasNext
        override def next(): List[A] = {
          val result = x.takeWhile(groupBy).toList
          if (x.hasNext) x.next()
          result
        }
      }
  }
}
