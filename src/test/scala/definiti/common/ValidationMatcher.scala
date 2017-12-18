package definiti.common

import definiti.native.{Invalid, Valid, Validation}
import org.scalatest.matchers.{MatchResult, Matcher}

trait ValidationMatcher {

  class ValidationMatcher[A](expected: Validation[A]) extends Matcher[Validation[A]] {

    def apply(left: Validation[A]): MatchResult = {
      (expected, left) match {
        case (Valid(expectedValue), Valid(gotValue)) =>
          if (expectedValue == gotValue) {
            success
          } else {
            failed(expected, left)
          }
        case (Valid(_), Invalid(_)) => failed(expected, left)
        case (Invalid(_), Valid(_)) => failed(expected, left)
        case (expectedInvalid: Invalid, gotInvalid: Invalid) =>
          if (expectedInvalid.errors.sortBy(_.path) == gotInvalid.errors.sortBy(_.path)) {
            success
          } else {
            failed(expected, left)
          }
      }
    }

    private def failed(expected: Validation[A], got: Validation[A]) = MatchResult(matches = false, s"${expected} did not equal ${got}", "")

    private val success = MatchResult(matches = true, "", "")
  }

  def beValidation[A](expected: Validation[A]): ValidationMatcher[A] = new ValidationMatcher[A](expected)
}

object ValidationMatcher extends ValidationMatcher
