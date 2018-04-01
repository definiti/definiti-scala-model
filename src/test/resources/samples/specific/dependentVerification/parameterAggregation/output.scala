import definiti.native._
import java.time.LocalDateTime

package object my {
  case class Parent(date: LocalDateTime, child: Child)
  object Parent {
    val verification: Verification[Parent] = Verification.all(Verification.all(Child.verification).from[Parent](_.child, "child"))
    def universe(now: LocalDateTime, minimalFromConfig: BigDecimal): Verification[Parent] = {
      object Parent0 extends SimpleVerification[Parent]("Date should be after now") {
        override def isValid(nominal: Parent): Boolean = DateExtension.isAfter(nominal.date, now)
      }
      Verification.all(Verification.all(Child.universe(now, minimalFromConfig)).from[Parent](_.child, "child"), Parent0)
    }
  }
  case class Child(date: LocalDateTime, grandChild: GrandChild)
  object Child {
    val verification: Verification[Child] = Verification.all(Verification.all(GrandChild.verification).from[Child](_.grandChild, "grandChild"))
    def universe(now: LocalDateTime, minimalFromConfig: BigDecimal): Verification[Child] = {
      object Child0 extends SimpleVerification[Child]("Date should be after tomorrow") {
        override def isValid(child: Child): Boolean = DateExtension.isAfter(child.date, DateExtension.plusDays(now, BigDecimal(1)))
      }
      Verification.all(Verification.all(GrandChild.universe(minimalFromConfig)).from[Child](_.grandChild, "grandChild"), Child0)
    }
  }
  case class GrandChild(value: BigDecimal)
  object GrandChild {
    val verification: Verification[GrandChild] = Verification.none[GrandChild]
    def universe(minimalFromConfig: BigDecimal): Verification[GrandChild] = {
      object GrandChild0 extends DefinedVerification[GrandChild] {
        private val message: String = "grandChild.universe"
        override def verify(grandChild: GrandChild): Option[Message] = {
          if (grandChild.value >= minimalFromConfig) None
          else Some(Message(message, grandChild.value, minimalFromConfig))
        }
      }
      Verification.all(GrandChild0)
    }
  }
}