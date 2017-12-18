package definiti.native

import java.time.LocalDateTime

import definiti.common.Generators
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

class VerificationSeasonSamplesSpec extends FlatSpec with Matchers with PropertyChecks {
  object SeasonModel {
    case class Season(period: Period, activities: List[String])
    case class Period(start: LocalDateTime, end: LocalDateTime)

    val periodInvalidMessage = "The start of the period should be before the end"
    val periodVerification = Verification(periodInvalidMessage) {
      period: Period => period.start.isBefore(period.end) || period.start.isEqual(period.end)
    }

    val activityInvalidMessage = "The activity cannot be empty"
    val nonEmptyActivityVerification = Verification(activityInvalidMessage) {
      activity: String => activity.nonEmpty
    }

    val activitiesInvalidMessage = "Please provide at least one activity"
    val atLeastOneActivityVerification = Verification(activitiesInvalidMessage) {
      activities: List[String] => activities.nonEmpty
    }

    val activitiesVerification = Verification.traverse(new ListVerification[String](nonEmptyActivityVerification), atLeastOneActivityVerification)

    val seasonVerification = Verification.traverse(
      periodVerification.from((x: Season) => x.period),
      activitiesVerification.from((x: Season) => x.activities)
    )
  }

  object SeasonGenerators {
    val atLeastOneEmptyActivityGen = for {
      validActivities <- Gen.listOf(Generators.nonEmptyString)
      invalidActivities <- Gen.nonEmptyListOf(Gen.const(""))
    } yield Random.shuffle(validActivities ++ invalidActivities)
  }

  import SeasonModel._
  import SeasonGenerators._

  "Verification - sample Season" should "validate a valid value" in {
    val cases = for {
      dates <- Generators.twoOrderedDates
      activities <- Gen.nonEmptyListOf(Generators.nonEmptyString)
    } yield Season(Period(dates._1, dates._2), activities)

    forAll(cases) { season =>
      seasonVerification.verify(season) should === (Valid(season))
    }
  }

  it should "return the period error when the period is invalid" in {
    val cases = for {
      dates <- Generators.twoOrderedDates.suchThat(x => !x._1.isEqual(x._2))
      activities <- Gen.nonEmptyListOf(Generators.nonEmptyString)
    } yield Season(Period(dates._2, dates._1), activities)

    forAll(cases) { season =>
      seasonVerification.verify(season) should === (Invalid(periodInvalidMessage))
    }
  }

  it should "return the activities error when there is no activity" in {
    val cases = for {
      dates <- Generators.twoOrderedDates
    } yield Season(Period(dates._1, dates._2), List.empty)

    forAll(cases) { season =>
      seasonVerification.verify(season) should === (Invalid(activitiesInvalidMessage))
    }
  }

  it should "return the activity error when there is at least one invalid activity" in {
    val cases = for {
      dates <- Generators.twoOrderedDates
      activities <- atLeastOneEmptyActivityGen
    } yield Season(Period(dates._1, dates._2), activities)

    forAll(cases) { season =>
      val result = seasonVerification.verify(season)
      result shouldBe an[Invalid]
      result.asInstanceOf[Invalid].errors.head should === (activityInvalidMessage)
    }
  }

  it should "return an invalid value for all invalid cases" in {
    val cases = for {
      dates <- Generators.twoOrderedDates.suchThat(x => !x._1.isEqual(x._2))
      activities <- Gen.oneOf(Gen.const(List.empty), atLeastOneEmptyActivityGen)
    } yield Season(Period(dates._2, dates._1), activities)

    forAll(cases) { season =>
      seasonVerification.verify(season) shouldBe an[Invalid]
    }
  }

  it should "return the invalid messages only (1)" in {
    val season = Season(
      Period(LocalDateTime.of(2018, 1, 1, 0, 0), LocalDateTime.of(2017, 12, 31, 23, 59)),
      List.empty
    )
    seasonVerification.verify(season) should === (Invalid(periodInvalidMessage, activitiesInvalidMessage))
  }

  it should "return the invalid messages only (2)" in {
    val season = Season(
      Period(LocalDateTime.of(2018, 1, 1, 0, 0), LocalDateTime.of(2017, 12, 31, 23, 59)),
      List("", "")
    )
    seasonVerification.verify(season) should === (Invalid(periodInvalidMessage, activityInvalidMessage, activityInvalidMessage))
  }
}
