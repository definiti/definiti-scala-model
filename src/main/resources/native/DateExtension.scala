package definiti.native

import java.util.Date // TODO: Use newer Date API

object DateExtension {
  def timestamp(date: Date): BigDecimal = date.getTime()

  def day(date: Date): BigDecimal = date.getDate

  def month(date: Date): BigDecimal = date.getMonth
}

class OrderedDateExtension(val inner: Date) extends AnyVal {
  def <(other: Date): Boolean = DateExtension.timestamp(inner) < DateExtension.timestamp(other)

  def >(other: Date): Boolean = DateExtension.timestamp(inner) > DateExtension.timestamp(other)

  def <=(other: Date): Boolean = DateExtension.timestamp(inner) <= DateExtension.timestamp(other)

  def >=(other: Date): Boolean = DateExtension.timestamp(inner) >= DateExtension.timestamp(other)
}