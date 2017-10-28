package definiti.scalamodel.builder

import definiti.core.ast.Verification
import definiti.scalamodel.ScalaAST

trait VerificationBuilder {
  self: ScalaModelBuilder =>

  def generateVerification(verification: Verification): ScalaAST.Statement = {
    ScalaAST
      .StatementsGroup(verification.comment.map(comment => ScalaAST.Comment(comment.trim)))
      .plus(generateVerificationObject(verification))
  }

  private def generateVerificationObject(verification: Verification): ScalaAST.Statement = {
    ScalaAST.ObjectDef(
      name = verification.name,
      body = Seq(generateApply(verification))
    )
  }

  private def generateApply(verification: Verification): ScalaAST.Statement = {
    ScalaAST.Def1(
      name = "apply",
      typ = s"Verification[${generateParameterType(verification.function.parameters.head.typeReference)}]",
      generics = Seq.empty,
      parameters = Seq(ScalaAST.Parameter(name = "message", typ = "String", defaultValue = Some(ScalaAST.StringExpression(verification.message)))),
      body = Some(generateMessageApplyBody(verification))
    )
  }

  private def generateMessageApplyBody(verification: Verification): ScalaAST.Expression = {
    ScalaAST.CallFunction(
      ScalaAST.CallFunction(
        ScalaAST.SimpleExpression("Verification"),
        Seq(ScalaAST.SimpleExpression("message"))
      ),
      Seq(generateLambda(verification.function))
    )
  }
}
