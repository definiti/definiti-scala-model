package definiti.scalamodel.builder

import definiti.core.ast._
import definiti.scalamodel.ScalaAST

trait ClassDefinitionBuilder {
  self: ScalaModelBuilder =>

  def generateClassDefinition(classDefinition: ClassDefinition): ScalaAST.Statement = classDefinition match {
    case definedType: DefinedType => generateDefinedType(definedType)
    case aliasType: AliasType => generateAliasType(aliasType)
  }


  private def generateDefinedType(definedType: DefinedType): ScalaAST.Statement = {
    val caseClass = generateCaseClass(definedType)
    val companionObject = generateDefinedTypeCompanionObject(definedType)
    ScalaAST.StatementsGroup(caseClass, companionObject)
  }

  private def generateCaseClass(definedType: DefinedType): ScalaAST.Statement = {
    ScalaAST.CaseClassDef(
      name = definedType.name,
      extendz = None,
      parameters = definedType.attributes.map(attributeAsParameter),
      body = Seq.empty,
      property = None
    )
  }

  private def attributeAsParameter(attributeDefinition: AttributeDefinition): ScalaAST.Parameter = {
    ScalaAST.Parameter(
      name = attributeDefinition.name,
      typ = generateParameterType(attributeDefinition.typeReference)
    )
  }

  private def generateDefinedTypeCompanionObject(definedType: DefinedType): ScalaAST.Statement = {
    val attributeVerifications = definedType.attributes.map(generateVerificationFromAttribute)
    val typeVerifications = generateVerificationFromDefinedType(definedType)
    val applyCheck = generateApplyCheck(definedType)
    ScalaAST.ObjectDef(
      name = definedType.name,
      body = attributeVerifications :+ typeVerifications :+ applyCheck
    )
  }

  private def generateVerificationFromAttribute(attributeDefinition: AttributeDefinition): ScalaAST.Statement = {
    ScalaAST.ClassVal(
      name = s"${attributeDefinition.name}Verification",
      typ = s"Verification[${generateParameterType(attributeDefinition.typeReference)}]",
      body = Seq(
        ScalaAST.CallMethod(
          target = ScalaAST.SimpleExpression("Verification"),
          name = "traverse",
          arguments = attributeDefinition.verifications.map(generateVerificationCall)
        )
      ),
      isLazy = false,
      isPrivate = false
    )
  }

  private def generateVerificationCall(verificationReference: VerificationReference): ScalaAST.Expression = {
    ScalaAST.CallFunction(
      target = ScalaAST.SimpleExpression(verificationReference.verificationName),
      arguments = verificationReference.message.map(ScalaAST.StringExpression).toSeq
    )
  }

  private def generateVerificationFromDefinedType(definedType: DefinedType): ScalaAST.Statement = {
    ScalaAST.ClassVal(
      name = s"${definedType.name}Verifications",
      typ = s"Verification[${definedType.name}]",
      body = Seq(
        ScalaAST.CallMethod(
          target = ScalaAST.SimpleExpression("Verification"),
          name = "traverse",
          arguments = definedType.verifications.map(generateVerificationFromTypeVerification)
        )
      ),
      isLazy = false,
      isPrivate = false
    )
  }

  private def generateVerificationFromTypeVerification(typeVerification: TypeVerification): ScalaAST.Expression = {
    ScalaAST.CallFunction(
      target = ScalaAST.CallFunction(
        target = ScalaAST.SimpleExpression("Verification"),
        arguments = Seq(ScalaAST.StringExpression(typeVerification.message))
      ),
      arguments = Seq(generateLambda(typeVerification.function))
    )
  }

  private def generateApplyCheck(definedType: DefinedType): ScalaAST.Statement = {
    ScalaAST.Def1(
      name = "applyCheck",
      typ = s"Validation[${definedType.name}]",
      generics = Seq.empty,
      parameters = definedType.attributes.map(attributeAsParameter),
      body = Some(
        ScalaAST.CallMethod(
          target = ScalaAST.CallMethod(
            target = ScalaAST.SimpleExpression("Validation"),
            name = "all",
            arguments = definedType.attributes.map { attribute =>
              ScalaAST.CallMethod(
                target = ScalaAST.SimpleExpression(s"${attribute.name}Verification"),
                name = "verify",
                arguments = Seq(ScalaAST.SimpleExpression(attribute.name))
              )
            }
          ),
          name = "andThen",
          arguments = Seq(
            ScalaAST.CallMethod(
              target = ScalaAST.SimpleExpression(s"${definedType.name}Verifications"),
              name = "verify",
              arguments = Seq(ScalaAST.CallFunction(
                target = ScalaAST.SimpleExpression(definedType.name),
                arguments = definedType.attributes.map(attribute => ScalaAST.SimpleExpression(attribute.name))
              ))
            )
          )
        )
      )
    )
  }

  private def generateAliasType(aliasType: AliasType): ScalaAST.Statement = {
    library.types(aliasType.alias.typeName) match {
      case definedType: DefinedType => generateDefinedAliasType(aliasType, definedType)
      case alias: NativeClassDefinition => generateNativeAliasType(aliasType, alias)
      case _ => throw new RuntimeException("Undefined type: " + aliasType.alias.readableString)
    }
  }

  private def generateDefinedAliasType(aliasType: AliasType, definedType: DefinedType): ScalaAST.Statement = {
    ScalaAST.ObjectDef(
      name = aliasType.name,
      body = Seq(
        generateAliasTypeVerifications(aliasType),
        generateDefinedAliasApplyCheck(aliasType, definedType)
      )
    )
  }

  private def generateAliasTypeVerifications(aliasType: AliasType): ScalaAST.Statement = {
    ScalaAST.ClassVal(
      name = s"${aliasType.name}Verifications",
      typ = s"Verification[${aliasType.alias.typeName}]",
      body = Seq(
        ScalaAST.CallMethod(
          target = ScalaAST.SimpleExpression("Verification"),
          name = "traverse",
          arguments = aliasType.inherited.map(generateVerificationCall)
        )
      ),
      isLazy = false,
      isPrivate = false
    )
  }

  private def generateDefinedAliasApplyCheck(aliasType: AliasType, definedType: DefinedType): ScalaAST.Statement = {
    ScalaAST.Def1(
      name = "applyCheck",
      typ = s"Validation[${definedType.name}]",
      generics = Seq.empty,
      parameters = definedType.attributes.map(attributeAsParameter),
      body = Some(
        ScalaAST.CallMethod(
          target = ScalaAST.CallMethod(
            target = ScalaAST.SimpleExpression(definedType.name),
            name = "applyCheck",
            arguments = definedType.attributes.map(attribute => ScalaAST.SimpleExpression(attribute.name))
          ),
          name = "flatMap",
          arguments = Seq(
            ScalaAST.CallAttribute(
              target = ScalaAST.SimpleExpression(s"${aliasType.name}Verifications"),
              name = "verify"
            )
          )
        )
      )
    )
  }

  private def generateNativeAliasType(aliasType: AliasType, alias: NativeClassDefinition): ScalaAST.Statement = {
    ScalaAST.ObjectDef(
      name = aliasType.name,
      body = Seq(
        generateAliasTypeVerifications(aliasType),
        generateNativeAliasApplyCheck(aliasType, alias)
      )
    )
  }

  private def generateNativeAliasApplyCheck(aliasType: AliasType, alias: NativeClassDefinition): ScalaAST.Statement = {
    ScalaAST.Def1(
      name = "applyCheck",
      typ = s"Validation[${alias.name}]",
      parameters = Seq(ScalaAST.Parameter("input", alias.name)),
      body = Some(
        ScalaAST.CallMethod(
          target = ScalaAST.SimpleExpression(s"${aliasType.name}Verifications"),
          name = "verify",
          arguments = Seq(ScalaAST.SimpleExpression("input"))
        )
      )
    )
  }
}
