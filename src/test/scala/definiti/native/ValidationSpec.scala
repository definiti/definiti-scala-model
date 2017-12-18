package definiti.native

import definiti.common.Generators
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class ValidationSpec extends FlatSpec with Matchers with PropertyChecks {
  "Valid.isValid" should "return true" in {
    forAll(Generators.anyInt) { n =>
      Valid(n).isValid should ===(true)
    }
  }

  "Valid.map" should "be a Valid value from the one returned by the given function" in {
    forAll(Generators.anyInt) { n =>
      Valid(n).map(x => x + x) should ===(Valid(n + n))
    }
  }

  it should "be transitive" in {
    forAll(Generators.anyInt) { n =>
      val result = Valid(n)
        .map(x => x + x)
        .map(x => x * x)
      result should ===(Valid((n + n) * (n + n)))
    }
  }

  "Valid.flatMap" should "be the Valid value returned by the function" in {
    forAll(Generators.anyInt) { n =>
      val result = Valid(n).flatMap(x => Valid(x + x))
      result should ===(Valid(n + n))
    }
  }

  it should "be transitive" in {
    forAll(Generators.anyInt) { n =>
      val result = Valid(n)
        .flatMap(x => Valid(x + x))
        .flatMap(x => Valid(x * x))
      result should ===(Valid((n + n) * (n + n)))
    }
  }

  it should "be an Invalid value with errors returned by the function" in {
    val cases = for {
      n <- Generators.anyInt
      message <- Generators.anyString
    } yield (n, message)

    forAll(cases) { case (n, message) =>
      val result = Valid(n).flatMap(x => Invalid.root(s"${x} ${message}"))
      result should ===(Invalid.root(s"${n} ${message}"))
    }
  }

  it should "stay an Invalid value by transition (map)" in {
    val cases = for {
      n <- Generators.anyInt
      message <- Generators.anyString
    } yield (n, message)

    forAll(cases) { case (n, message) =>
      val result = Valid(n)
        .flatMap[Int](x => Invalid.root(s"${x} ${message}"))
        .map(x => x * x)
      result should ===(Invalid.root(s"${n} ${message}"))
    }
  }

  it should "stay an Invalid value by transition (flatMap)" in {
    val cases = for {
      n <- Generators.anyInt
      message <- Generators.anyString
    } yield (n, message)

    forAll(cases) { case (n, message) =>
      val result = Valid(n)
        .flatMap[Int](x => Invalid.root(s"${x} ${message}"))
        .flatMap(x => Valid(x * x))
      result should ===(Invalid.root(s"${n} ${message}"))
    }
  }

  "Valid.andThen" should "return the given Valid value" in {
    forAll(Generators.anyInt) { n =>
      val result = Valid(n).andThen(Valid(n + 1))
      result should ===(Valid(n + 1))
    }
  }

  it should "return the given Invalid value" in {
    val cases = for {
      n <- Generators.anyInt
      message <- Generators.anyString
    } yield (n, message)

    forAll(cases) { case (n, message) =>
      val result = Valid(n).andThen(Invalid.root(message))
      result should ===(Invalid.root(message))
    }
  }

  "Invalid.isValid" should "return false" in {
    forAll(Generators.anyString) { message =>
      Invalid.root(message).isValid should ===(false)
    }
  }

  "Invalid.map" should "be a Invalid with the same error" in {
    forAll(Generators.anyString) { message =>
      val result = invalid[Int](message).map(x => x + x)
      result should ===(Invalid.root(message))
    }
  }

  it should "be transitive" in {
    forAll(Generators.anyString) { message =>
      val result = invalid[Int](message)
        .map(x => x + x)
        .map(x => x * x)
      result should ===(Invalid.root(message))
    }
  }

  "Invalid.flatMap" should "be the initial Invalid value" in {
    forAll(Generators.anyString) { message =>
      val result = invalid[Int](message).flatMap(x => Valid(x + x))
      result should ===(Invalid.root(message))
    }
  }

  it should "be transitive" in {
    forAll(Generators.anyString) { message =>
      val result = invalid[Int](message)
        .flatMap(x => Valid(x + x))
        .flatMap(x => Valid(x * x))
      result should ===(Invalid.root(message))
    }
  }

  it should "stay the initial Invalid" in {
    val cases = for {
      messages <- Generators.anyString
      otherMessage <- Generators.anyString
    } yield (messages, otherMessage)
    forAll(cases) { case (message, otherMessage) =>
      val result = invalid[Int](message).flatMap(x => Invalid.root(s"${x} ${otherMessage}"))
      result should ===(Invalid.root(message))
    }
  }

  "Invalid.andThen" should "return the initial Invalid value" in {
    val cases = for {
      messages <- Generators.anyString
      otherMessage <- Generators.anyString
    } yield (messages, otherMessage)
    forAll(cases) { case (message, otherMessage) =>
      val result = Invalid.root(message).andThen(Invalid.root(otherMessage))
      result should ===(Invalid.root(message))
    }
  }

  it should "not be changed into a valid value" in {
    forAll(Generators.anyString) { message =>
      val result = Invalid.root(message).andThen(Valid(1))
      result should ===(Invalid.root(message))
    }
  }

  private def invalid[A](errorMessage: String): Validation[A] = Invalid.root(errorMessage)
}
