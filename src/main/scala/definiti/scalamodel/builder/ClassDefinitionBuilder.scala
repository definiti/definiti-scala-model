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
      typ = generateType(attributeDefinition.typeReference)
    )
  }

  private def generateDefinedTypeCompanionObject(definedType: DefinedType): ScalaAST.Statement = {
    val attributeVerifications = definedType.attributes.map(generateVerificationFromAttribute)
    val typeVerifications = generateVerificationFromDefinedType(definedType)
    val allVerifications = generateAllVerificationFromDefinedType(definedType)
    val applyCheck = generateApplyCheck(definedType)
    ScalaAST.ObjectDef(
      name = definedType.name,
      body = attributeVerifications :+ typeVerifications :+ allVerifications :+ applyCheck
    )
  }

  private def generateVerificationFromAttribute(attributeDefinition: AttributeDefinition): ScalaAST.Statement = {
    val typeVerification = generateTypeVerificationCall(attributeDefinition.typeReference)
    val directVerifications = attributeDefinition.verifications.map(generateVerificationCall)
    val inheritedVerifications = verificationsFromTypeReference(attributeDefinition.typeReference).map(generateVerificationCall)
    ScalaAST.ClassVal(
      name = s"${attributeDefinition.name}Verification",
      typ = s"Verification[${generateType(attributeDefinition.typeReference)}]",
      body = Seq(
        ScalaAST.CallMethod(
          target = ScalaAST.SimpleExpression("Verification"),
          name = "traverse",
          arguments = typeVerification.toSeq ++ directVerifications ++ inheritedVerifications
        )
      ),
      isLazy = false,
      isPrivate = false
    )
  }

  private def generateTypeVerificationCall(typeReference: TypeReference): Option[ScalaAST.Expression] = {
    library.types.get(typeReference.typeName) match {
      case Some(aliasType: AliasType) =>
        Some(ScalaAST.CallAttribute(ScalaAST.SimpleExpression(typeReference.typeName), s"${aliasType.name}Verifications"))
      case Some(_: DefinedType) =>
        Some(ScalaAST.CallAttribute(ScalaAST.SimpleExpression(typeReference.typeName), s"allVerifications"))
      case _ => None
    }
  }

  private def generateVerificationCall(verificationReference: VerificationReference): ScalaAST.Expression = {
    ScalaAST.CallFunction(
      target = ScalaAST.SimpleExpression(verificationReference.verificationName),
      arguments = verificationReference.message.map(ScalaAST.StringExpression).toSeq
    )
  }

  private def generateVerificationFromDefinedType(definedType: DefinedType): ScalaAST.Statement = {
    val internalVerifications = internalVerificationsFromType(definedType).map(generateVerificationFromTypeVerification)
    val inheritedVerifications = verificationsFromType(definedType).map(generateVerificationCall)
    ScalaAST.ClassVal(
      name = s"${definedType.name}Verifications",
      typ = s"Verification[${definedType.name}]",
      body = Seq(
        ScalaAST.CallMethod(
          target = ScalaAST.SimpleExpression("Verification"),
          name = "traverse",
          arguments = internalVerifications ++ inheritedVerifications
        )
      ),
      isLazy = false,
      isPrivate = false
    )
  }

  private def generateAllVerificationFromDefinedType(definedType: DefinedType): ScalaAST.Statement = {
    val attributeArguments = definedType.attributes.map { attribute =>
      ScalaAST.CallMethod(
        target = ScalaAST.SimpleExpression(s"${attribute.name}Verification"),
        name = "from",
        arguments = Seq(ScalaAST.Lambda(
          parameters = Seq(ScalaAST.Parameter("x", typ = definedType.name)),
          body = ScalaAST.CallAttribute(ScalaAST.SimpleExpression("x"), attribute.name)
        ))
      )
    }
    ScalaAST.ClassVal(
      name = s"allVerifications",
      typ = s"Verification[${definedType.name}]",
      body = Seq(
        ScalaAST.CallMethod(
          target = ScalaAST.CallMethod(
            target = ScalaAST.SimpleExpression("Verification"),
            name = "traverse",
            arguments = attributeArguments
          ),
          name = "andThen",
          arguments = Seq(ScalaAST.SimpleExpression(s"${definedType.name}Verifications"))
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
          target = ScalaAST.SimpleExpression("allVerifications"),
          name = "verify",
          arguments = Seq(ScalaAST.CallFunction(
            target = ScalaAST.SimpleExpression(definedType.name),
            arguments = definedType.attributes.map(attribute => ScalaAST.SimpleExpression(attribute.name))
          ))
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
    val typeVerification = generateTypeVerificationCall(aliasType.alias)
    val directVerifications = aliasType.inherited.map(generateVerificationCall)
    val inheritedVerifications = verificationsFromTypeReference(aliasType.alias).map(generateVerificationCall)
    val internalVerifications = internalVerificationsFromType(aliasType).map(generateVerificationFromTypeVerification)
    ScalaAST.ClassVal(
      name = s"${aliasType.name}Verifications",
      typ = s"Verification[${generateType(aliasType.alias)}]",
      body = Seq(
        ScalaAST.CallMethod(
          target = ScalaAST.SimpleExpression("Verification"),
          name = "traverse",
          arguments = typeVerification.toSeq ++ directVerifications ++ inheritedVerifications ++ internalVerifications
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
