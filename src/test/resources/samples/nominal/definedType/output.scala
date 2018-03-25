import definiti.native._
import java.time.LocalDateTime

package object root {
  case class MyType(myAttribute: String)
  object MyType {
    val verification: Verification[MyType] = Verification.all()
  }
}