package definiti.native

import java.time.LocalDateTime

import definiti.utils.Generators
import definiti.utils.ValidationMatcher
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

class VerificationSeasonSamplesSpec extends FlatSpec with Matchers with PropertyChecks with ValidationMatcher {
  import VerificationSeasonSamplesSpec._

  "Verification - sample Season" should "validate a valid value" in {
    val cases = for {
      dates <- Generators.twoOrderedDates
      activities <- Gen.nonEmptyListOf(Generators.nonEmptyString)
    } yield Season(Period(dates._1, dates._2), activities, List.empty)

    forAll(cases) { season =>
      Season.seasonVerification.verify(season) should === (Valid(season))
    }
  }

  it should "return the period error when the period is invalid" in {
    val cases = for {
      dates <- Generators.twoOrderedDates.suchThat(x => !x._1.isEqual(x._2))
      activities <- Gen.nonEmptyListOf(Generators.nonEmptyString)
    } yield Season(Period(dates._2, dates._1), activities, List.empty)

    forAll(cases) { season =>
      Season.seasonVerification.verify(season) should === (Invalid("period", Message(Period.periodInvalidMessage)))
    }
  }

  it should "return the activities error when there is no activity" in {
    val cases = for {
      dates <- Generators.twoOrderedDates
    } yield Season(Period(dates._1, dates._2), List.empty, List.empty)

    forAll(cases) { season =>
      Season.seasonVerification.verify(season) should === (Invalid("activities", Message(Season.activitiesInvalidMessage)))
    }
  }

  it should "return the activity error when there is at least one invalid activity" in {
    val cases = for {
      dates <- Generators.twoOrderedDates
      activities <- atLeastOneEmptyActivityGen
    } yield Season(Period(dates._1, dates._2), activities, List.empty)

    forAll(cases) { season =>
      val result = Season.seasonVerification.verify(season)
      val expectedErrors = season.activities.zipWithIndex.filter(_._1.isEmpty).map { case (_, n) => Error(s"activities[${n}]", Message(Season.activityInvalidMessage)) }
      result should beValidation[Season](Invalid(expectedErrors))
    }
  }

  it should "return an invalid value for all invalid cases" in {
    val cases = for {
      dates <- Generators.twoOrderedDates.suchThat(x => !x._1.isEqual(x._2))
      activities <- Gen.oneOf(Gen.const(List.empty), atLeastOneEmptyActivityGen)
    } yield Season(Period(dates._2, dates._1), activities, List.empty)

    forAll(cases) { season =>
      Season.seasonVerification.verify(season) shouldBe an[Invalid]
    }
  }

  it should "return the invalid messages only (1)" in {
    val season = Season(
      Period(LocalDateTime.of(2018, 1, 1, 0, 0), LocalDateTime.of(2017, 12, 31, 23, 59)),
      List.empty,
      List.empty
    )
    val expected = Invalid(
      Error("period", Message(Period.periodInvalidMessage)),
      Error("activities", Message(Season.activitiesInvalidMessage))
    )
    Season.seasonVerification.verify(season) should beValidation[Season](expected)
  }

  it should "return the invalid messages only (2)" in {
    val season = Season(
      Period(LocalDateTime.of(2018, 1, 1, 0, 0), LocalDateTime.of(2017, 12, 31, 23, 59)),
      List("", ""),
      List.empty
    )
    val expected = Invalid(
      Error("period", Message(Period.periodInvalidMessage)),
      Error("activities[0]", Message(Season.activityInvalidMessage)),
      Error("activities[1]", Message(Season.activityInvalidMessage))
    )
    Season.seasonVerification.verify(season) should beValidation[Season](expected)
  }

  it should "return the invalid messages only (3)" in {
    val season = Season(
      Period(LocalDateTime.of(2018, 1, 1, 0, 0), LocalDateTime.of(2017, 12, 31, 23, 59)),
      List("", ""),
      List(SeasonOption(name = "", description = ""), SeasonOption(name = "valid", description = ""))
    )
    val expected = Invalid(
      Error("period", Message(Period.periodInvalidMessage)),
      Error("activities[0]", Message(Season.activityInvalidMessage)),
      Error("activities[1]", Message(Season.activityInvalidMessage)),
      Error("options[0].name", Message(SeasonOption.nameInvalidMessage))
    )
    Season.seasonVerification.verify(season) should beValidation[Season](expected)
  }
}

object VerificationSeasonSamplesSpec {
  case class Season(period: Period, activities: List[String], options: List[SeasonOption])

  object Season {
    val activityInvalidMessage = "The activity cannot be empty"
    val nonEmptyActivityVerification = new SimpleVerification[String](activityInvalidMessage) {
      override def isValid(activity: String) = activity.nonEmpty
    }

    val activitiesInvalidMessage = "Please provide at least one activity"
    val atLeastOneActivityVerification = new SimpleVerification[Seq[String]](activitiesInvalidMessage) {
      override def isValid(activities: Seq[String]) = activities.nonEmpty
    }

    val activitiesVerification = Verification.all(new ListVerification[String](nonEmptyActivityVerification), atLeastOneActivityVerification)

    val optionsVerification = new ListVerification[SeasonOption](SeasonOption.seasonOptionVerification)

    val seasonVerification = Verification.all(
      Verification.definedVerificationToVerification(Period.periodVerification).from((x: Season) => x.period, "period"),
      activitiesVerification.from((x: Season) => x.activities, "activities"),
      optionsVerification.from((x: Season) => x.options, "options")
    )
  }

  case class Period(start: LocalDateTime, end: LocalDateTime)

  object Period {
    val periodInvalidMessage = "The start of the period should be before the end"
    val periodVerification = new SimpleVerification[Period](periodInvalidMessage) {
      override def isValid(period: Period) = period.start.isBefore(period.end) || period.start.isEqual(period.end)
    }
  }

  case class SeasonOption(name: String, description: String)

  object SeasonOption {
    val nameInvalidMessage = "The option name is empty"
    val nameVerification = new SimpleVerification[String](nameInvalidMessage) {
      override def isValid(name: String) = name.nonEmpty
    }

    val seasonOptionVerification = Verification.all(Verification.definedVerificationToVerification(nameVerification).from((x: SeasonOption) => x.name, "name"))
  }

  val atLeastOneEmptyActivityGen = for {
    validActivities <- Gen.listOf(Generators.nonEmptyString)
    invalidActivities <- Gen.nonEmptyListOf(Gen.const(""))
  } yield Random.shuffle(validActivities ++ invalidActivities)
}