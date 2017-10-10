package definiti.scalamodel.builder

import definiti.core.ast.Verification
import definiti.scalamodel.ScalaAST

trait VerificationBuilder {
  self: ScalaModelBuilder =>

  def generateVerification(verification: Verification): ScalaAST.Statement = {
    ScalaAST.StatementsGroup(verification.comment.map(comment => ScalaAST.Comment(comment.trim)))
      .plus(generateVerificationObject(verification))
      .plus(generateVerificationFunction(verification))
  }

  private def generateVerificationObject(verification: Verification): ScalaAST.Val = {
    ScalaAST.Val(
      name = verificationObjectName(verification),
      value = ScalaAST.CallFunction(
        ScalaAST.CallFunction(
          ScalaAST.SimpleExpression("Verification"),
          Seq(ScalaAST.SimpleExpression('"' + verification.message + '"'))
        ),
        Seq(generateLambda(verification.function))
      ),
      isLazy = true
    )
  }

  private def verificationObjectName(verification: Verification): String = {
    s"${verification.name}Verification"
  }

  private def generateVerificationFunction(verification: Verification): ScalaAST.Def1 = {
    ScalaAST.Def1(
      name = verificationFunctionName(verification),
      typ = s"Validation[${generateParameterType(verification.function.parameters.head.typeReference)}]",
      parameters = verification.function.parameters.map(generateParameter),
      body = Some(ScalaAST.CallMethod(
        target = ScalaAST.SimpleExpression(verificationObjectName(verification)),
        name = "verify",
        arguments = verification.function.parameters.map(parameter => ScalaAST.SimpleExpression(parameter.name))
      ))
    )
  }

  private def verificationFunctionName(verification: Verification): String = {
    s"verify${verification.name}"
  }
}
