package definiti.scalamodel.utils

import scala.collection.concurrent.Map
import java.util.concurrent.ConcurrentHashMap
import scala.collection.JavaConverters._

class Memoizer[A] {
  private val values: Map[String, A] = new ConcurrentHashMap[String, A]().asScala

  def value(key: String)(compute: => A): A = {
    values.getOrElseUpdate(key, compute)
  }
}
