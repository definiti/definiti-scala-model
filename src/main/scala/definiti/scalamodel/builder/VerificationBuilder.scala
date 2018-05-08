package definiti.scalamodel.builder

import definiti.common.ast._
import definiti.scalamodel.ScalaAST

trait VerificationBuilder {
  self: ScalaModelBuilder =>

  def generateVerification(verification: Verification): Seq[ScalaAST.TopLevelElement] = {
    verification.comment.map(comment => ScalaAST.Comment(comment.trim)) ++
      generateVerificationClass(verification)
  }

  private def generateVerificationClass(verification: Verification): ScalaAST.TopLevelElement = {
    val verificationBuilderStrategy = getVerificationBuilderStrategy(verification)
    ScalaAST.ClassDef(
      name = verification.name,
      generics = verification.function.genericTypes,
      extendz = verificationBuilderStrategy.extendz,
      parameters = verification.parameters.map(generateParameter) ++ verificationBuilderStrategy.messageParameter,
      body = verificationBuilderStrategy.verificationDef,
      property = None,
      privateConstructor = false
    )
  }

  private def getVerificationBuilderStrategy(verification: Verification): VerificationBuilderStrategy = {
    verification.message match {
      case literalMessage: LiteralMessage => new SimpleVerificationBuilder(verification, literalMessage)
      case typedMessage: TypedMessage => new ComplexVerificationBuilder(verification, typedMessage)
    }
  }

  private def generateVerificationType(verification: Verification): ScalaAST.Type = {
    verification.function.parameters.headOption match {
      case Some(ParameterDefinition(_, typeReference: TypeReference, _)) => generateScalaType(typeReference)
      case x => sys.error(s"Unexpected type ${x}")
    }
  }

  private sealed trait VerificationBuilderStrategy {
    def extendz: ScalaAST.Extends

    def verificationDef: ScalaAST.Def1

    def messageParameter: ScalaAST.Parameter
  }

  private class SimpleVerificationBuilder(verification: Verification, message: LiteralMessage) extends VerificationBuilderStrategy {
    override def extendz: ScalaAST.Extends = {
      ScalaAST.Extends(
        typ = ScalaAST.Type("SimpleVerification", generateVerificationType(verification)),
        parameters = ScalaAST.SimpleExpression("message")
      )
    }

    override def verificationDef: ScalaAST.Def1 = {
      generateDef("isValid", verification.function.copy(genericTypes = Seq.empty), "override")
    }

    override def messageParameter: ScalaAST.Parameter = {
      ScalaAST.Parameter(
        name = "message",
        typ = "String",
        defaultValue = ScalaAST.StringExpression(message.message)
      )
    }
  }

  private class ComplexVerificationBuilder(verification: Verification, message: TypedMessage) extends VerificationBuilderStrategy {
    override def extendz: ScalaAST.Extends = {
      ScalaAST.Extends(
        typ = ScalaAST.Type("DefinedVerification", generateVerificationType(verification)),
        parameters = Seq.empty
      )
    }

    override def verificationDef: ScalaAST.Def1 = {
      generateDef("verify", verification.function.copy(genericTypes = Seq.empty), "override")
    }

    override def messageParameter: ScalaAST.Parameter = {
      ScalaAST.Parameter(
        name = "message",
        typ = "String",
        defaultValue = ScalaAST.StringExpression(message.message)
      )
    }
  }
}