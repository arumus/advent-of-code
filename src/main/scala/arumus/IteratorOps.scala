package arumus

object IteratorOps {
  implicit class GroupIterator[A](x: Iterator[A]) {
    def groupedBy(groupBy: A => Boolean): Iterator[List[A]] =
      new Iterator[List[A]] {
        private val buffer = new collection.mutable.ListBuffer[A]

        override def hasNext: Boolean = buffer.nonEmpty || x.hasNext

        override def next(): List[A] = {
          while x.hasNext do {
            val next = x.next()
            if groupBy(next) then {
              buffer += next
            } else {
              val result = buffer.toList
              buffer.clear()
              buffer += next
              return result
            }
          }
          val result = buffer.toList
          buffer.clear()
          result
        }
      }
  }
}
