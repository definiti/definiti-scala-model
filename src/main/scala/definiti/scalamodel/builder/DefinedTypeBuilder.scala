package definiti.scalamodel.builder

import definiti.core.ast._
import definiti.scalamodel.ScalaAST
import definiti.scalamodel.utils.{ListUtils, StringUtils}

trait DefinedTypeBuilder {
  self: ScalaModelBuilder =>

  def generateDefinedType(definedType: DefinedType): Seq[ScalaAST.TopLevelElement] = {
    Seq(
      generateCaseClass(definedType),
      generateCompanionObject(definedType)
    )
  }

  private def generateCaseClass(definedType: DefinedType): ScalaAST.TopLevelElement = {
    ScalaAST.CaseClassDef(
      name = definedType.name,
      extendz = None,
      parameters = definedType.attributes.map(attributeAsParameter),
      generics = definedType.genericTypes
    )
  }

  def attributeAsParameter(attributeDefinition: AttributeDefinition): ScalaAST.Parameter = {
    ScalaAST.Parameter(
      name = attributeDefinition.name,
      typ = generateType(attributeDefinition.typeDeclaration).toCode
    )
  }

  def generateCompanionObject(definedType: DefinedType): ScalaAST.TopLevelElement = {
    ScalaAST.ObjectDef(
      name = definedType.name,
      extendz = None,
      body = Seq(generatePublicVerification(definedType)),
      property = None
    )
  }

  private def generatePublicVerification(definedType: DefinedType): ScalaAST.Statement = {
    if (definedType.parameters.nonEmpty || definedType.genericTypes.nonEmpty) {
      ScalaAST.Def1(
        name = "verification",
        typ = ScalaAST.Type("Verification", generateType(definedType)).toCode,
        generics = definedType.genericTypes,
        parameters = definedType.parameters.map(generateParameter),
        body = Some(ScalaAST.Block(generateInternalVerificationObjects(definedType) :+ generateVerification(definedType))),
        property = None
      )
    } else {
      ScalaAST.ClassVal(
        name = "verification",
        typ = ScalaAST.Type("Verification", generateType(definedType)).toCode,
        body = generateInternalVerificationObjects(definedType) :+ generateVerification(definedType)
      )
    }
  }

  private def generateVerification(definedType: DefinedType): ScalaAST.Statement = {
    val attributeVerifications = definedType.attributes.flatMap(generateAttributeVerifications(_, definedType))
    val inheritedVerifications = generateInheritedVerifications(definedType)
    val internalVerifications = generateInternalVerifications(definedType)
    val verifications = attributeVerifications ++ inheritedVerifications ++ internalVerifications
    ScalaAST.CallMethod(
      "Verification",
      "all",
      verifications: _*
    )
  }

  private def generateAttributeVerifications(attributeDefinition: AttributeDefinition, definedType: DefinedType): Option[ScalaAST.Expression] = {
    val typeVerification = generateTypeVerificationCall(attributeDefinition.typeDeclaration)
    val directVerifications = attributeDefinition.verifications.map(generateVerificationCall(_, definedType))
    val deepVerification = generateDeepVerification(attributeDefinition.typeDeclaration)
    val verifications = typeVerification ++ directVerifications ++ deepVerification
    val groupVerificationOpt = generateGroupVerification(attributeDefinition.typeDeclaration, verifications.toSeq)
    val verificationFromType = groupVerificationOpt.map { groupVerification =>
      ScalaAST.CallMethod(
        target = groupVerification,
        name = s"from[${generateType(definedType).toCode}]",
        arguments = Seq(
          ScalaAST.SimpleExpression(s"_.${attributeDefinition.name}"),
          ScalaAST.StringExpression(attributeDefinition.name)
        )
      )
    }
    verificationFromType
  }

  private def generateTypeVerificationCall(typeDeclaration: TypeDeclaration): Option[ScalaAST.Expression] = {
    library.typesMap.get(typeDeclaration.typeName) match {
      case Some(_: AliasType | _: DefinedType) =>
        if (typeDeclaration.parameters.nonEmpty) {
          Some(ScalaAST.CallMethod(
            target = ScalaAST.SimpleExpression(StringUtils.lastPart(typeDeclaration.typeName)),
            name = s"verification${generateGenericTypes(typeDeclaration.genericTypes)}",
            arguments = typeDeclaration.parameters.map(generateExpression)
          ))
        } else {
          Some(ScalaAST.CallAttribute(
            target = ScalaAST.SimpleExpression(StringUtils.lastPart(typeDeclaration.typeName)),
            name = s"verification${generateGenericTypes(typeDeclaration.genericTypes)}"
          ))
        }
      case _ => None
    }
  }

  private def generateDeepVerification(typeDeclaration: TypeDeclaration): Option[ScalaAST.Expression] = {
    val innerVerification = typeDeclaration.genericTypes.flatMap(generateTypeVerificationCall)
    if (innerVerification.nonEmpty) {
      if (isList(typeDeclaration)) {
        Some(ScalaAST.New(s"ListVerification", Seq.empty, innerVerification))
      } else if (isOption(typeDeclaration)) {
        Some(ScalaAST.New(s"OptionVerification", Seq.empty, innerVerification))
      } else {
        None
      }
    } else {
      None
    }
  }

  private def generateGroupVerification(typeReference: TypeReference, verifications: Seq[ScalaAST.Expression]): Option[ScalaAST.Expression] = {
    generateGroupVerification(generateType(typeReference), verifications)
  }

  private def generateGroupVerification(typ: String, verifications: Seq[ScalaAST.Expression]): Option[ScalaAST.Expression] = {
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

  private def generateInheritedVerifications(definedType: DefinedType): Seq[ScalaAST.Expression] = {
    verificationsFromType(definedType)
      .map(generateVerificationCall(_, definedType))
  }

  private def generateVerificationCall(verificationReference: VerificationReference, definedType: DefinedType): ScalaAST.Expression = {
    val verification = library.verificationsMap(verificationReference.verificationName)
    ScalaAST.New(
      name = verification.name,
      generics = ListUtils.replaceOrdered(verification.function.genericTypes, definedType.genericTypes),
      arguments = verificationReference.parameters.map(generateExpression)
    )
  }

  private def generateInternalVerifications(definedType: DefinedType): Seq[ScalaAST.Expression] = {
    definedType.verifications.indices.map { index =>
      ScalaAST.SimpleExpression(s"${definedType.name}${index}")
    }
  }

  private def generateInternalVerificationObjects(definedType: DefinedType): Seq[ScalaAST.Statement] = {
    definedType.verifications.zipWithIndex.map { case (typeVerification, index) =>
      generateTypeVerificationObject(s"${definedType.name}${index}", typeVerification)
    }
  }
}
