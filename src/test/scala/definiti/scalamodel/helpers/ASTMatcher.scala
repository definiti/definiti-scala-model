package definiti.scalamodel.helpers

import definiti.common.program.{Ko, Ok, ProgramResult}
import definiti.common.validation.Alert
import definiti.scalamodel.ScalaAST.Root
import org.scalatest.matchers.{MatchResult, Matcher}

trait ASTMatcher {

  class ValidatedRootMatcher(expected: ProgramResult[Root]) extends Matcher[ProgramResult[Root]] {

    def apply(left: ProgramResult[Root]): MatchResult = {
      (expected, left) match {
        case (Ok(expectedValue, _), Ok(gotValue, _)) =>
          if (expectedValue == gotValue) {
            success
          } else {
            failed(expected, left)
          }
        case (Ok(_, _), Ko(_)) => failed(expected, left)
        case (Ko(_), Ok(_, _)) => failed(expected, left)
        case (Ko(expectedErrors), Ko(gotErrors)) =>
          val missingErrors = missingExpectedErrors(expectedErrors, gotErrors)
          val additionalErrors = additionalGotErrors(expectedErrors, gotErrors)
          if (missingErrors.nonEmpty || additionalErrors.nonEmpty) {
            failed(expected, left)
          } else {
            success
          }
      }
    }

    private def failed(expected: ProgramResult[Root], got: ProgramResult[Root]) = MatchResult(matches = false, s"${expected.prettyPrint} did not equal ${got.prettyPrint}", "")

    private def missingExpectedErrors(expectedErrors: Seq[Alert], gotErrors: Seq[Alert]): Seq[Alert] = {
      expectedErrors.filter(expectedError => !gotErrors.contains(expectedError))
    }

    private def additionalGotErrors(expectedErrors: Seq[Alert], gotErrors: Seq[Alert]): Seq[Alert] = {
      gotErrors.filter(gotError => !expectedErrors.contains(gotError))
    }

    private val success = MatchResult(matches = true, "", "")
  }

  def beValidRoot(expected: Root) = new ValidatedRootMatcher(Ok(expected))

}

object ASTMatcher extends ASTMatcher
