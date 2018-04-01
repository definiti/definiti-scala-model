import definiti.native._
import java.time.LocalDateTime

package object my {
  case class Nominal(date: LocalDateTime)
  object Nominal {
    val verification: Verification[Nominal] = Verification.none[Nominal]
    def universe(now: LocalDateTime): Verification[Nominal] = {
      object Nominal0 extends SimpleVerification[Nominal]("Date should be after now") {
        override def isValid(nominal: Nominal): Boolean = DateExtension.isAfter(nominal.date, now)
      }
      Verification.all(Nominal0)
    }
  }
}