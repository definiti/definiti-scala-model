import definiti.native._
import java.time.LocalDateTime

package object my {
  case class Parent(date: LocalDateTime, child: Child)
  object Parent {
    val verification: Verification[Parent] = Verification.all(Verification.all(Child.verification).from[Parent](_.child, "child"))
    def withConfig(minimalFromConfig: BigDecimal): Verification[Parent] = Verification.all(Verification.all(Child.withConfig(minimalFromConfig)).from[Parent](_.child, "child"))
    def withNow(now: LocalDateTime): Verification[Parent] = {
      object Parent0 extends SimpleVerification[Parent]("Date should be after now") {
        override def isValid(nominal: Parent): Boolean = DateExtension.isAfter(nominal.date, now)
      }
      Verification.all(Verification.all(Child.withNow(now)).from[Parent](_.child, "child"), Parent0)
    }
  }
  case class Child(date: LocalDateTime, grandChild: GrandChild)
  object Child {
    val verification: Verification[Child] = Verification.all(Verification.all(GrandChild.verification).from[Child](_.grandChild, "grandChild"))
    def withConfig(minimalFromConfig: BigDecimal): Verification[Child] = Verification.all(Verification.all(GrandChild.withConfig(minimalFromConfig)).from[Child](_.grandChild, "grandChild"))
    def withNow(now: LocalDateTime): Verification[Child] = {
      object Child0 extends SimpleVerification[Child]("Date should be after tomorrow") {
        override def isValid(child: Child): Boolean = DateExtension.isAfter(child.date, DateExtension.plusDays(now, BigDecimal(1)))
      }
      Verification.all(Child0)
    }
  }
  case class GrandChild(value: BigDecimal)
  object GrandChild {
    val verification: Verification[GrandChild] = Verification.none[GrandChild]
    def withConfig(minimalFromConfig: BigDecimal): Verification[GrandChild] = {
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