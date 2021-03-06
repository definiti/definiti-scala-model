package definiti.scalamodel.utils

import scala.collection.mutable.ListBuffer

object ListUtils {
  def replaceOrdered[A](source: Seq[A], replacements: Seq[A]): Seq[A] = {
    val destination = ListBuffer[A]()
    for (i <- source.indices) {
      if (replacements.length > i) {
        destination.append(replacements(i))
      } else {
        destination.append(source(i))
      }
    }
    destination.toList
  }

  def distinctBy[A, B](seq: Seq[A], key: A => B): Seq[A] = {
    seq
      .groupBy(key)
      .map(_._2.head)
      .toSeq
  }
}
