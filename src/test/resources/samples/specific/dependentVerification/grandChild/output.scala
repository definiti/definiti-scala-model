import definiti.native._
import java.time.LocalDateTime

package object my {
  case class Parent(date: LocalDateTime, child: Child)
  object Parent {
    val verification: Verification[Parent] = Verification.all(Verification.all(Child.verification).from[Parent](_.child, "child"))
    def universe(now: LocalDateTime): Verification[Parent] = {
      object Parent0 extends SimpleVerification[Parent]("Date should be after now") {
        override def isValid(nominal: Parent): Boolean = DateExtension.isAfter(nominal.date, now)
      }
      Verification.all(Verification.all(Child.universe(now)).from[Parent](_.child, "child"), Parent0)
    }
  }
  case class Child(date: LocalDateTime, grandChild: GrandChild)
  object Child {
    val verification: Verification[Child] = Verification.all(Verification.all(GrandChild.verification).from[Child](_.grandChild, "grandChild"))
    def universe(now: LocalDateTime): Verification[Child] = {
      object Child0 extends SimpleVerification[Child]("Date should be after tomorrow") {
        override def isValid(child: Child): Boolean = DateExtension.isAfter(child.date, DateExtension.plusDays(now, BigDecimal(1)))
      }
      Verification.all(Verification.all(GrandChild.universe(now)).from[Child](_.grandChild, "grandChild"), Child0)
    }
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