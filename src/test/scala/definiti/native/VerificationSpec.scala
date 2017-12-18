package definiti.native

import definiti.common.{Generators, ValidationMatcher}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

class VerificationSpec extends FlatSpec with Matchers with PropertyChecks with ValidationMatcher {
  "NoVerification.verify" should "return a Valid value" in {
    forAll(Generators.anyString) { value =>
      new NoVerification[String].verify(value) should ===(Valid(value))
    }
  }

  "ValueVerification.verify" should "return a Valid value when the condition is valid" in {
    val cases = for {
      n <- Generators.positiveInt
      message <- Generators.anyString
    } yield (n, message)

    forAll(cases) { case (n, message) =>
      new ValueVerification[Int](x => x > 0, message).verify(n) should === (Valid(n))
    }
  }

  it should "return an Invalid value when the condition is invalid" in {
    val cases = for {
      n <- Generators.negativeInt
      message <- Generators.anyString
    } yield (n, message)

    forAll(cases) { case (n, message) =>
      new ValueVerification[Int](x => x > 0, message).verify(n) should === (Invalid.root(message))
    }
  }

  "ListVerification.verify" should "return a Valid value when the condition is valid for all values" in {
    val cases = for {
      values <- Gen.listOf(Generators.positiveInt)
      message <- Generators.anyString
    } yield (values, message)

    forAll(cases) { case (values, message) =>
      val result = new ListVerification[Int](new ValueVerification(x => x > 0, message)).verify(values)
      result should === (Valid(values))
    }
  }

  it should "return an Invalid value when the condition is invalid for all elements" in {
    val cases = for {
      values <- Gen.nonEmptyListOf(Generators.negativeInt)
      message <- Generators.anyString
    } yield (values, message)

    forAll(cases) { case (values, message) =>
      val result = new ListVerification[Int](new ValueVerification(x => x > 0, message)).verify(values)
      val expectedErrors = values.indices.map{ index => Error(s"[${index}]", message) }
      result should beValidation[List[Int]](Invalid(expectedErrors))
    }
  }

  it should "return an Invalid value when the condition is invalid for at least one element" in {
    val cases = for {
      validValues <- Gen.listOf(Generators.positiveInt)
      invalidValues <- Gen.nonEmptyListOf(Generators.negativeInt)
      message <- Generators.anyString
    } yield (validValues, invalidValues, message)

    forAll(cases) { case (validValues, invalidValues, message) =>
      val values = Random.shuffle(validValues ++ invalidValues)
      val result: Validation[List[Int]] = new ListVerification[Int](new ValueVerification(x => x > 0, message)).verify(values)
      val expectedErrors = values.zipWithIndex.filter(_._1 < 0).map{ case (_, index) => Error(s"[${index}]", message) }
      result should beValidation[List[Int]](Invalid(expectedErrors))
    }
  }

  "OptionVerification.verify" should "return a Valid value when the condition is valid for Some(element)" in {
    val cases = for {
      n <- Generators.positiveInt
      message <- Generators.anyString
    } yield (Some(n), message)

    forAll(cases) { case (nOption, message) =>
      val result = new OptionVerification[Int](new ValueVerification(x => x > 0, message)).verify(nOption)
      result should === (Valid(nOption))
    }
  }

  it should "return a Valid value when the Option is empty" in {
    forAll(Generators.anyString) { message =>
      val result = new OptionVerification[Int](new ValueVerification(x => x > 0, message)).verify(None)
      result should === (Valid(None))
    }
  }

  it should "return an Invalid value when the condition is invalid for the element" in {
    val cases = for {
      value <- Generators.negativeInt
      message <- Generators.anyString
    } yield (Some(value), message)

    forAll(cases) { case (nOption, message) =>
      val result = new OptionVerification[Int](new ValueVerification(x => x > 0, message)).verify(nOption)
      result should === (Invalid.root(message))
    }
  }
}
