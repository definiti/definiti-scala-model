import definiti.native._
import java.time.LocalDateTime

package object my {
  case class Nominal(date: LocalDateTime)
  object Nominal {
    val verification: Verification[Nominal] = {
      object Nominal0 extends SimpleVerification[Nominal]("Timestamp should be upper than 0") {
        override def isValid(nominal: Nominal): Boolean = {
          DateExtension.timestamp(nominal.date) > BigDecimal(0)
        }
      }
      Verification.all(Nominal0)
    }
    def universe(now: LocalDateTime): Verification[Nominal] = {
      object Nominal0 extends SimpleVerification[Nominal]("Date should be after now") {
        override def isValid(nominal: Nominal): Boolean = DateExtension.isAfter(nominal.date, now)
      }
      Verification.all(Nominal0)
    }
  }
}