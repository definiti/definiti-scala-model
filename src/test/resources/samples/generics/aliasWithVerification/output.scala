import definiti.native._
import java.time.LocalDateTime

package object root {
  class ListIsNonEmpty[A](message: String = "The list should not be empty") extends SimpleVerification[Seq[A]](message) {
    override def isValid(list: Seq[A]): Boolean = ListExtension.nonEmpty(list)
  }
  object NonEmptyStringList {
    val verification: Verification[Seq[String]] = Verification.all(new ListIsNonEmpty[String]())
  }
}