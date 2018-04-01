import definiti.native._
import java.time.LocalDateTime

package object root {
  class AlwaysTrue(message: String = "Never fail") extends SimpleVerification[String](message) {
    override def isValid(x: String): Boolean = true
  }
}