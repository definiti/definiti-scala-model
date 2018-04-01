import definiti.native._
import java.time.LocalDateTime

package object my {
  case class Parent(date: LocalDateTime, child: Child)
  object Parent {
    val verification: Verification[Parent] = Verification.all(Verification.all(Child.verification).from[Parent](_.child, "child"))
    def universe(now: LocalDateTime): Verification[Parent] = Verification.all(Verification.all(Child.universe(now)).from[Parent](_.child, "child"))
  }
  case class Child(date: LocalDateTime, grandChild: GrandChild)
  object Child {
    val verification: Verification[Child] = Verification.all(Verification.all(GrandChild.verification).from[Child](_.grandChild, "grandChild"))
    def universe(now: LocalDateTime): Verification[Child] = Verification.all(Verification.all(GrandChild.universe(now)).from[Child](_.grandChild, "grandChild"))
  }
  case class GrandChild(date: LocalDateTime)
  object GrandChild {
    val verification: Verification[GrandChild] = Verification.none[GrandChild]
    def universe(now: LocalDateTime): Verification[GrandChild] = {
      object GrandChild0 extends SimpleVerification[GrandChild]("Date should be after yesterday") {
        override def isValid(grandChild: GrandChild): Boolean = DateExtension.isAfter(grandChild.date, DateExtension.minusDays(now, BigDecimal(1)))
      }
      Verification.all(GrandChild0)
    }
  }
}