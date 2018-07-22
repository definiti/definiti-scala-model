import definiti.native._
import java.time.LocalDateTime

package object my {
  class IsNonEmpty[A](message: String = "This list should not be empty") extends SimpleVerification[Seq[A]](message) {
    override def isValid(list: Seq[A]): Boolean = ListExtension.nonEmpty(list)
  }
  case class MyType(attribute: Seq[String])
  object MyType {
    val verification: Verification[MyType] = Verification.all(Verification.all(new my.IsNonEmpty[String]()).from[MyType](_.attribute, "attribute"))
  }
}