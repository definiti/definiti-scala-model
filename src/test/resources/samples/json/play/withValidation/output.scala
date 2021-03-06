import definiti.native._
import java.time.LocalDateTime
import play.api.libs.json._
import definiti.native.JsonPlaySupport._

package object my {
  case class MyFirstType(myAttribute: String)
  object MyFirstType {
    val verification: Verification[MyFirstType] = Verification.none[MyFirstType]
    val rawFormat: OFormat[MyFirstType] = Json.format[MyFirstType]
    implicit val format: OFormat[MyFirstType] = formatWithValidation(rawFormat, verification)
  }
  case class MySecondType(myFirstAttribute: BigDecimal, mySecondAttribute: MyFirstType, myThirdAttribute: Seq[MyFirstType], myFourthAttribute: Option[MyFirstType], myFifthAttribute: Seq[MyThirdType], mySixthAttribute: Seq[MyThirdType])
  object MySecondType {
    val verification: Verification[MySecondType] = Verification.all(Verification.all(MyFirstType.verification).from[MySecondType](_.mySecondAttribute, "mySecondAttribute"), Verification.all(new ListVerification(MyFirstType.verification)).from[MySecondType](_.myThirdAttribute, "myThirdAttribute"), Verification.all(new OptionVerification(MyFirstType.verification)).from[MySecondType](_.myFourthAttribute, "myFourthAttribute"), Verification.all(AliasList.verification[MyThirdType], new ListVerification(MyThirdType.verification)).from[MySecondType](_.myFifthAttribute, "myFifthAttribute"), Verification.all(ListOfThird.verification).from[MySecondType](_.mySixthAttribute, "mySixthAttribute"))
    val rawFormat: OFormat[MySecondType] = {
      implicit val MyFirstTypeFormat = MyFirstType.rawFormat
      implicit val MyThirdTypeFormat = MyThirdType.rawFormat
      Json.format[MySecondType]
    }
    implicit val format: OFormat[MySecondType] = formatWithValidation(rawFormat, verification)
  }
  case class MyThirdType(myAttribute: String)
  object MyThirdType {
    val verification: Verification[MyThirdType] = Verification.none[MyThirdType]
    val rawFormat: OFormat[MyThirdType] = Json.format[MyThirdType]
    implicit val format: OFormat[MyThirdType] = formatWithValidation(rawFormat, verification)
  }
  object AliasList {
    def verification[A](): Verification[Seq[A]] = Verification.none[Seq[A]]
  }
  object ListOfThird {
    val verification: Verification[Seq[MyThirdType]] = Verification.none[Seq[MyThirdType]]
  }
}
