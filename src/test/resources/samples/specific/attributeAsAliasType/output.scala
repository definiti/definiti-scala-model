import definiti.native._
import java.time.LocalDateTime

package object my {
  class IsRequired(message: String = "This string is required") extends SimpleVerification[String](message) {
    override def isValid(string: String): Boolean = StringExtension.nonEmpty(string)
  }
  case class MyType(attribute: String)
  object MyType {
    val verification: Verification[MyType] = Verification.all(Verification.all(RequiredString.verification).from[MyType](_.attribute, "attribute"))
  }
  object RequiredString {
    val verification: Verification[String] = Verification.all(new my.IsRequired())
  }
}