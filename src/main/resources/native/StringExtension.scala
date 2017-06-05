package definiti.native

import scala.collection.immutable.StringOps

object StringExtension {
  @inline def nonEmpty(string: String): Boolean = string.nonEmpty

  @inline def trim(string: String): String = string.trim

  @inline def startsWith(string: String, prefix: String): Boolean = string.startsWith(prefix)

  @inline def matches(string: String, regex: String): Boolean = string.matches(regex)
}
