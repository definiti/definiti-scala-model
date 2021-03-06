package definiti.scalamodel.utils

import definiti.common.utils.{StringUtils => CoreStringUtils}

object StringUtils {
  def lastPart(source: String, separator: Char = '.'): String = CoreStringUtils.lastPart(source, separator)

  def excludeLastPart(source: String, separator: Char = '.'): String = {
    if (source.isEmpty) {
      source
    } else if (source.last == separator) {
      source.substring(0, source.length - 1)
    } else if (source.contains(separator)) {
      source.substring(0, source.lastIndexOf(separator))
    } else {
      ""
    }
  }

  def withLastPartExtracted(source: String, separator: Char): (String, String) = {
    (excludeLastPart(source, separator), lastPart(source, separator))
  }

  def prefixOnLastPart(source: String, separator: Char, prefix: String): String = {
    if (source.contains(separator)) {
      val (firstPart, lastPart) = withLastPartExtracted(source, separator)
      firstPart + separator + prefix + lastPart
    } else {
      prefix + source
    }
  }
}
