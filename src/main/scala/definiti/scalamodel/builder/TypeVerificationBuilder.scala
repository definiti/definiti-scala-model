package definiti.scalamodel.builder

import definiti.core.ast._
import definiti.scalamodel.ScalaAST

trait TypeVerificationBuilder {
  self: ScalaModelBuilder =>

  def generateTypeVerificationObject(name: String, typeVerification: TypeVerification): ScalaAST.TopLevelElement = {
    val typeVerificationBuilderStrategy = typeVerification.message match {
      case literalMessage: LiteralMessage => new SimpleTypeBuilder(name, typeVerification, literalMessage)
      case typedMessage: TypedMessage => new ComplexTypeBuilder(name, typeVerification, typedMessage)
    }

    ScalaAST.ObjectDef(
      name = name,
      extendz = Some(typeVerificationBuilderStrategy.extendz),
      body = Seq(
        typeVerificationBuilderStrategy.messageVal,
        typeVerificationBuilderStrategy.verificationDef
      ),
      property = Some("private")
    )
  }

  private def generateTypeVerificationType(typeVerification: TypeVerification): ScalaAST.Type = {
    typeVerification.function.parameters.headOption match {
      case Some(ParameterDefinition(_, typeReference: TypeReference, _)) => generateScalaType(typeReference)
      case x => sys.error(s"Unexpected type ${x}")
    }
  }

  private sealed trait TypeVerificationBuilderStrategy {
    def extendz: ScalaAST.Extends

    def messageVal: ScalaAST.ClassVal

    def verificationDef: ScalaAST.Def1
  }

  private class SimpleTypeBuilder(name: String, typeVerification: TypeVerification, message: LiteralMessage) extends TypeVerificationBuilderStrategy {
    override def extendz: ScalaAST.Extends = {
      ScalaAST.Extends(
        typ = ScalaAST.Type("SimpleVerification", generateTypeVerificationType(typeVerification)),
        parameters = Seq(ScalaAST.StringExpression(message.message))
      )
    }

    override def messageVal: ScalaAST.ClassVal = {
      ScalaAST.ClassVal(
        name = "message",
        typ = "String",
        body = Seq(ScalaAST.StringExpression(message.message)),
        isPrivate = true
      )
    }

    override def verificationDef: ScalaAST.Def1 = {
      generateDef("isValid", typeVerification.function.copy(genericTypes = Seq.empty), Some("override"))
    }
  }

  private class ComplexTypeBuilder(name: String, typeVerification: TypeVerification, message: TypedMessage) extends TypeVerificationBuilderStrategy {
    override def extendz: ScalaAST.Extends = {
      ScalaAST.Extends(
        typ = ScalaAST.Type("DefinedVerification", generateTypeVerificationType(typeVerification)),
        parameters = Seq.empty
      )
    }

    override def messageVal: ScalaAST.ClassVal = {
      ScalaAST.ClassVal(
        name = "message",
        typ = "String",
        body = Seq(ScalaAST.StringExpression(message.message)),
        isPrivate = true
      )
    }

    override def verificationDef: ScalaAST.Def1 = {
      generateDef("verify", typeVerification.function.copy(genericTypes = Seq.empty), Some("override"))
    }
  }
}
