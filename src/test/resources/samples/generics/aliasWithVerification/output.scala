import definiti.native._
import java.time.LocalDateTime

package object root {
  class ListIsNonEmpty[A](message: String = "The list should not be empty") extends SimpleVerification[List[A]](message) {
    override def isValid(list: List[A]): Boolean = ListExtension.nonEmpty(list)
  }
  object NonEmptyStringList {
    val verification: Verification[List[String]] = Verification.all(new ListIsNonEmpty[String]())
  }
}