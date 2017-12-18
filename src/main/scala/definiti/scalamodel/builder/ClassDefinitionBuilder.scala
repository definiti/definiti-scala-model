package definiti.scalamodel.builder

import definiti.core.ast._
import definiti.scalamodel.ScalaAST
import definiti.scalamodel.ScalaAST.Expression

trait ClassDefinitionBuilder {
  self: ScalaModelBuilder =>

  def generateClassDefinition(classDefinition: ClassDefinition): Seq[ScalaAST.TopLevelElement] = classDefinition match {
    case definedType: DefinedType => generateDefinedType(definedType)
    case aliasType: AliasType => generateAliasType(aliasType)
  }


  private def generateDefinedType(definedType: DefinedType): Seq[ScalaAST.TopLevelElement] = {
    val caseClass = generateCaseClass(definedType)
    val companionObject = generateDefinedTypeCompanionObject(definedType)
    caseClass ++ companionObject
  }

  private def generateCaseClass(definedType: DefinedType): Seq[ScalaAST.TopLevelElement] = {
    Seq(
      ScalaAST.CaseClassDef(
        name = definedType.name,
        extendz = None,
        parameters = definedType.attributes.map(attributeAsParameter),
        generics = definedType.genericTypes
      )
    )
  }

  private def attributeAsParameter(attributeDefinition: AttributeDefinition): ScalaAST.Parameter = {
    ScalaAST.Parameter(
      name = attributeDefinition.name,
      typ = generateType(attributeDefinition.typeReference)
    )
  }

  private def generateDefinedTypeCompanionObject(definedType: DefinedType): Seq[ScalaAST.TopLevelElement] = {
    val attributeVerifications = definedType.attributes.map(generateVerificationFromAttribute(_, definedType.genericTypes))
    val typeVerifications = generateVerificationFromDefinedType(definedType)
    val allVerifications = generateAllVerificationFromDefinedType(definedType)
    val applyCheck = generateApplyCheck(definedType)
    val jsonSupport = buildJsonConverter(definedType)
    Seq(
      ScalaAST.ObjectDef(
        name = definedType.name,
        body = (attributeVerifications :+ typeVerifications :+ allVerifications :+ applyCheck) ++ jsonSupport
      )
    )
  }

  private def generateVerificationFromAttribute(attributeDefinition: AttributeDefinition, outerGenerics: Seq[String]): ScalaAST.Statement = {
    val typeVerification = generateTypeVerificationCall(attributeDefinition.typeReference)
    val directVerifications = attributeDefinition.verifications.map(generateVerificationCall(_, generateGenericTypes(attributeDefinition.typeReference.genericTypes)))
    val inheritedVerifications = verificationsFromTypeReference(attributeDefinition.typeReference).map(generateVerificationCall)
    val deepVerifications = generateDeepVerification(attributeDefinition.typeReference)
    val genericTypes = extractGenericsFromTypeReference(attributeDefinition.typeReference, outerGenerics)
    generateDefOrVal(
      name = s"${attributeDefinition.name}Verification",
      typ = s"Verification[${generateType(attributeDefinition.typeReference)}]",
      genericTypes = genericTypes,
      body = ScalaAST.CallMethod(
        target = ScalaAST.SimpleExpression("Verification"),
        name = "traverse",
        arguments = typeVerification.toSeq ++ directVerifications ++ inheritedVerifications ++ deepVerifications
      )
    )
  }

  private def extractGenericsFromTypeReference(typeReference: TypeReference, outerGenerics: Seq[String]): Seq[String] = {
    def containsGeneric(typeReference: TypeReference, generic: String): Boolean = {
      typeReference.typeName == generic || typeReference.genericTypes.exists(containsGeneric(_, generic))
    }

    outerGenerics.filter(containsGeneric(typeReference, _))
  }

  private def generateTypeVerificationCall(typeReference: TypeReference): Option[ScalaAST.Expression] = {
    library.types.get(typeReference.typeName) match {
      case Some(aliasType: AliasType) =>
        Some(ScalaAST.CallAttribute(ScalaAST.SimpleExpression(typeReference.typeName), s"${aliasType.name}Verifications${generateGenericTypes(typeReference.genericTypes)}"))
      case Some(_: DefinedType) =>
        Some(ScalaAST.CallAttribute(ScalaAST.SimpleExpression(typeReference.typeName), s"allVerifications${generateGenericTypes(typeReference.genericTypes)}"))
      case _ => None
    }
  }

  private def generateVerificationCall(verificationReference: VerificationReference): ScalaAST.Expression = {
    generateVerificationCall(verificationReference, "")
  }

  private def generateVerificationCall(verificationReference: VerificationReference, generics: String): ScalaAST.Expression = {
    ScalaAST.CallFunction(
      target = ScalaAST.SimpleExpression(s"${verificationReference.verificationName}${generics}"),
      arguments = verificationReference.message.map(ScalaAST.StringExpression).toSeq
    )
  }

  private def generateDeepVerification(typeReference: TypeReference): Option[ScalaAST.Expression] = {
    if (isList(typeReference)) {
      Some(ScalaAST.New(
        name = s"ListVerification${generateGenericTypes(typeReference.genericTypes)}",
        arguments = typeReference.genericTypes.flatMap(generateTypeVerificationCall)
      ))
    } else if (isOption(typeReference)) {
      Some(ScalaAST.New(
        name = s"OptionVerification${generateGenericTypes(typeReference.genericTypes)}",
        arguments = typeReference.genericTypes.flatMap(generateTypeVerificationCall)
      ))
    } else {
      None
    }
  }

  private def generateVerificationFromDefinedType(definedType: DefinedType): ScalaAST.Statement = {
    val internalVerifications = internalVerificationsFromType(definedType).map(generateVerificationFromTypeVerification)
    val inheritedVerifications = verificationsFromType(definedType).map(generateVerificationCall)
    generateDefOrVal(
      name = s"${definedType.name}Verifications",
      typ = s"Verification[${definedType.name}${generateGenerics(definedType.genericTypes)}]",
      genericTypes = definedType.genericTypes,
      body = ScalaAST.CallMethod(
        target = ScalaAST.SimpleExpression("Verification"),
        name = "traverse",
        arguments = internalVerifications ++ inheritedVerifications
      )
    )
  }

  private def generateAllVerificationFromDefinedType(definedType: DefinedType): ScalaAST.Statement = {
    val attributeArguments = definedType.attributes.map { attribute =>
      ScalaAST.CallMethod(
        target = ScalaAST.SimpleExpression(s"${attribute.name}Verification"),
        name = "from",
        arguments = Seq(
          ScalaAST.Lambda(
            parameters = Seq(ScalaAST.Parameter("x", typ = s"${definedType.name}${generateGenerics(definedType.genericTypes)}")),
            body = ScalaAST.CallAttribute(ScalaAST.SimpleExpression("x"), attribute.name)
          ),
          ScalaAST.StringExpression(attribute.name)
        )
      )
    }
    generateDefOrVal(
      name = s"allVerifications",
      typ = s"Verification[${definedType.name}${generateGenerics(definedType.genericTypes)}]",
      genericTypes = definedType.genericTypes,
      body = ScalaAST.CallMethod(
        target = ScalaAST.CallMethod(
          target = ScalaAST.SimpleExpression("Verification"),
          name = "traverse",
          arguments = attributeArguments
        ),
        name = "andThen",
        arguments = Seq(ScalaAST.SimpleExpression(s"${definedType.name}Verifications"))
      )
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
    val generics = generateGenerics(definedType.genericTypes)
    ScalaAST.Def1(
      name = s"applyCheck${generics}",
      typ = s"Validation[${definedType.name}${generics}]",
      generics = Seq.empty,
      parameters = definedType.attributes.map(attributeAsParameter),
      body = Some(
        ScalaAST.CallMethod(
          target = ScalaAST.SimpleExpression(s"allVerifications${generics}"),
          name = "verify",
          arguments = Seq(ScalaAST.CallFunction(
            target = ScalaAST.SimpleExpression(definedType.name),
            arguments = definedType.attributes.map(attribute => ScalaAST.SimpleExpression(attribute.name))
          ))
        )
      )
    )
  }

  private def generateAliasType(aliasType: AliasType): Seq[ScalaAST.TopLevelElement] = {
    library.types(aliasType.alias.typeName) match {
      case definedType: DefinedType => generateDefinedAliasType(aliasType, definedType)
      case alias: NativeClassDefinition => generateNativeAliasType(aliasType, alias)
      case _ => throw new RuntimeException("Undefined type: " + aliasType.alias.readableString)
    }
  }

  private def generateDefinedAliasType(aliasType: AliasType, definedType: DefinedType): Seq[ScalaAST.TopLevelElement] = {
    Seq(
      ScalaAST.ObjectDef(
        name = aliasType.name,
        body = Seq(
          generateAliasTypeVerifications(aliasType),
          generateDefinedAliasApplyCheck(aliasType, definedType)
        )
      )
    )
  }

  private def generateAliasTypeVerifications(aliasType: AliasType): ScalaAST.Statement = {
    val typeVerification = generateTypeVerificationCall(aliasType.alias)
    val inheritedVerifications = verificationsFromType(aliasType).map(generateVerificationCall(_, generateGenericsOfTypes(aliasType.alias)))
    val internalVerifications = internalVerificationsFromType(aliasType).map(generateVerificationFromTypeVerification)
    generateDefOrVal(
      name = s"${aliasType.name}Verifications",
      typ = s"Verification[${generateType(aliasType.alias)}]",
      genericTypes = aliasType.genericTypes,
      body = ScalaAST.CallMethod(
        target = ScalaAST.SimpleExpression("Verification"),
        name = "traverse",
        arguments = typeVerification.toSeq ++ inheritedVerifications ++ internalVerifications
      )
    )
  }

  private def generateDefOrVal(name: String, typ: String, genericTypes: Seq[String], body: Expression): ScalaAST.Statement = {
    if (genericTypes.nonEmpty) {
      ScalaAST.Def0(
        name = name,
        typ = typ,
        generics = genericTypes,
        body = Some(body)
      )
    } else {
      ScalaAST.ClassVal(
        name = name,
        typ = typ,
        body = Seq(body)
      )
    }
  }

  private def generateDefinedAliasApplyCheck(aliasType: AliasType, definedType: DefinedType): ScalaAST.Statement = {
    val generics = generateGenerics(aliasType.genericTypes)
    ScalaAST.Def1(
      name = s"applyCheck${generics}",
      typ = s"Validation[${definedType.name}${generics}]",
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
              target = ScalaAST.SimpleExpression(s"${aliasType.name}Verifications${generics}"),
              name = "verify"
            )
          )
        )
      )
    )
  }

  private def generateNativeAliasType(aliasType: AliasType, alias: NativeClassDefinition): Seq[ScalaAST.TopLevelElement] = {
    Seq(
      ScalaAST.ObjectDef(
        name = aliasType.name,
        body = Seq(
          generateAliasTypeVerifications(aliasType),
          generateNativeAliasApplyCheck(aliasType, alias)
        )
      )
    )
  }

  private def generateNativeAliasApplyCheck(aliasType: AliasType, alias: NativeClassDefinition): ScalaAST.Statement = {
    val generics = generateGenerics(aliasType.genericTypes)
    val fullType = generateType(aliasType.alias)
    ScalaAST.Def1(
      name = s"applyCheck${generics}",
      typ = s"Validation[$fullType]",
      parameters = Seq(ScalaAST.Parameter("input", fullType)),
      body = Some(
        ScalaAST.CallMethod(
          target = ScalaAST.SimpleExpression(s"${aliasType.name}Verifications${generics}"),
          name = "verify",
          arguments = Seq(ScalaAST.SimpleExpression("input"))
        )
      )
    )
  }
}
