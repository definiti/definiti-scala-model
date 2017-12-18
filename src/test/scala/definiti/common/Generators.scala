package definiti.common

import com.fortysevendeg.scalacheck.datetime.jdk8.GenJdk8
import org.scalacheck.Gen

object Generators {
  val anyInt = Gen.chooseNum(Int.MinValue / 2, Int.MaxValue / 2)
  val positiveInt = Gen.posNum[Int]
  val negativeInt = Gen.negNum[Int]

  val anyString = Gen.alphaStr
  val nonEmptyString = for {
    firstChar <- Gen.alphaChar
    str <- Gen.alphaStr
  } yield firstChar + str

  val anyDate = GenJdk8.genZonedDateTime.map(_.toLocalDateTime)
  val twoOrderedDates = for {
    date1 <- anyDate
    date2 <- anyDate
  } yield {
    if (date1.isBefore(date2)) {
      (date1, date2)
    } else {
      (date2, date1)
    }
  }
}
