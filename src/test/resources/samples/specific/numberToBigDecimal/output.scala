import definiti.native._
import java.time.LocalDateTime

package object numbers {
  def twice(n: BigDecimal): BigDecimal = {
    n + n
  }
  def sum(values: Seq[BigDecimal]): BigDecimal = ListExtension.foldLeft(values, BigDecimal(0), (acc: BigDecimal, current: BigDecimal) => acc + current)
}