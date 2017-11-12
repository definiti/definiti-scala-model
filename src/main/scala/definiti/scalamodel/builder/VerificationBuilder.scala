package definiti.scalamodel.builder

import definiti.core.ast.Verification
import definiti.scalamodel.ScalaAST

trait VerificationBuilder {
  self: ScalaModelBuilder =>

  def generateVerification(verification: Verification): Seq[ScalaAST.TopLevelElement] = {
    verification.comment.map(comment => ScalaAST.Comment(comment.trim)).toSeq :+ generateVerificationObject(verification)
  }

  private def generateVerificationObject(verification: Verification): ScalaAST.TopLevelElement = {
    ScalaAST.ObjectDef(
      name = verification.name,
      body = Seq(generateApply(verification))
    )
  }

  private def generateApply(verification: Verification): ScalaAST.Statement = {
    ScalaAST.Def1(
      name = "apply",
      typ = s"Verification[${generateType(verification.function.parameters.head.typeReference)}]",
      generics = verification.function.genericTypes,
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
