import definiti.native._
import java.time.LocalDateTime
import spray.json.RootJsonFormat
import definiti.native.JsonSpraySupport._

package object my {
  case class MyFirstType(myAttribute: String)
  object MyFirstType {
    val verification: Verification[MyFirstType] = Verification.all()
    implicit val rawFormat: RootJsonFormat[MyFirstType] = jsonFormat1(MyFirstType.apply)
  }
  case class MySecondType(myFirstAttribute: BigDecimal, mySecondAttribute: MyFirstType, myThirdAttribute: List[MyFirstType], myFourthAttribute: Option[MyFirstType], myFifthAttribute: List[MyThirdType], mySixthAttribute: List[MyThirdType])
  object MySecondType {
    val verification: Verification[MySecondType] = Verification.all(Verification.all(MyFirstType.verification).from[MySecondType](_.mySecondAttribute, "mySecondAttribute"), Verification.all(new ListVerification(MyFirstType.verification)).from[MySecondType](_.myThirdAttribute, "myThirdAttribute"), Verification.all(new OptionVerification(MyFirstType.verification)).from[MySecondType](_.myFourthAttribute, "myFourthAttribute"), Verification.all(AliasList.verification[MyThirdType], new ListVerification(MyThirdType.verification)).from[MySecondType](_.myFifthAttribute, "myFifthAttribute"), Verification.all(ListOfThird.verification).from[MySecondType](_.mySixthAttribute, "mySixthAttribute"))
    implicit val rawFormat: RootJsonFormat[MySecondType] = {
      implicit val MyFirstTypeFormat = MyFirstType.rawFormat
      implicit val MyThirdTypeFormat = MyThirdType.rawFormat
      jsonFormat6(MySecondType.apply)
    }
  }
  case class MyThirdType(myAttribute: String)
  object MyThirdType {
    val verification: Verification[MyThirdType] = Verification.all()
    implicit val rawFormat: RootJsonFormat[MyThirdType] = jsonFormat1(MyThirdType.apply)
  }
  object AliasList {
    def verification[A](): Verification[List[A]] = {
      Verification.none[List[A]]
    }
  }
  object ListOfThird {
    val verification: Verification[List[MyThirdType]] = Verification.none[List[MyThirdType]]
  }
}
