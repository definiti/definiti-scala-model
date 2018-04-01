package definiti.scalamodel.builder.typeVerification

import definiti.core.ast._
import definiti.scalamodel.ScalaAST
import definiti.scalamodel.builder.ScalaModelBuilder

trait TypeVerificationBuilder
  extends AtomicVerificationBuilder
    with DependentVerificationBuilder
    with TypeVerificationInformation {
  self: ScalaModelBuilder =>

  def generateGroupVerification(typeReference: TypeReference, verifications: Seq[ScalaAST.Expression]): Option[ScalaAST.Expression] = {
    generateGroupVerification(generateType(typeReference), verifications)
  }

  def generateGroupVerification(typ: String, verifications: Seq[ScalaAST.Expression]): Option[ScalaAST.Expression] = {
    verifications match {
      case Nil => None
      case _ =>
        Some(
          ScalaAST.CallMethod(
            target = ScalaAST.SimpleExpression("Verification"),
            name = "all",
            arguments = verifications
          )
        )
    }
  }

  def generateTypeVerificationObject(name: String, typeVerification: AtomicTypeVerification): ScalaAST.TopLevelElement = {
    val typeVerificationBuilderStrategy = typeVerification.message match {
      case literalMessage: LiteralMessage => new SimpleTypeBuilder(name, typeVerification, literalMessage)
      case typedMessage: TypedMessage => new ComplexTypeBuilder(name, typeVerification, typedMessage)
    }

    ScalaAST.ObjectDef(
      name = name,
      extendz = Some(typeVerificationBuilderStrategy.extendz),
      body = typeVerificationBuilderStrategy.messageVal.toSeq :+ typeVerificationBuilderStrategy.verificationDef,
      property = Some("private")
    )
  }

  private def generateTypeVerificationType(typeVerification: TypeVerification): ScalaAST.Type = {
    typeVerification.function.parameters.headOption match {
      case Some(ParameterDefinition(_, typeReference: TypeReference, _)) => generateScalaType(typeReference)
      case x => sys.error(s"Unexpected type ${x}")
    }
  }

  def generateDependentTypeVerificationObject(name: String, typeVerification: DependentTypeVerification): ScalaAST.TopLevelElement = {
    val typeVerificationBuilderStrategy = typeVerification.message match {
      case literalMessage: LiteralMessage => new SimpleTypeBuilder(name, typeVerification, literalMessage)
      case typedMessage: TypedMessage => new ComplexTypeBuilder(name, typeVerification, typedMessage)
    }

    ScalaAST.ObjectDef(
      name = name,
      extendz = Some(typeVerificationBuilderStrategy.extendz),
      body =
        typeVerificationBuilderStrategy.messageVal.toSeq :+
          typeVerificationBuilderStrategy.verificationDef.copy(
            parameters = Seq(typeVerificationBuilderStrategy.verificationDef.parameters.head)
          ),
      property = None
    )
  }

  private sealed trait TypeVerificationBuilderStrategy {
    def extendz: ScalaAST.Extends

    def messageVal: Option[ScalaAST.ClassVal]

    def verificationDef: ScalaAST.Def1
  }

  private class SimpleTypeBuilder(name: String, typeVerification: TypeVerification, message: LiteralMessage) extends TypeVerificationBuilderStrategy {
    override def extendz: ScalaAST.Extends = {
      ScalaAST.Extends(
        typ = ScalaAST.Type("SimpleVerification", generateTypeVerificationType(typeVerification)),
        parameters = Seq(ScalaAST.StringExpression(message.message))
      )
    }

    override def messageVal: Option[ScalaAST.ClassVal] = None

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

    override def messageVal: Option[ScalaAST.ClassVal] = {
      Some(
        ScalaAST.ClassVal(
          name = "message",
          typ = "String",
          body = Seq(ScalaAST.StringExpression(message.message)),
          isPrivate = true
        )
      )
    }

    override def verificationDef: ScalaAST.Def1 = {
      generateDef("verify", typeVerification.function.copy(genericTypes = Seq.empty), Some("override"))
    }
  }
}
