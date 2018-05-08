package definiti.scalamodel.builder.typeVerification

import definiti.common.ast._
import definiti.scalamodel.ScalaAST
import definiti.scalamodel.builder.ScalaModelBuilder
import definiti.scalamodel.model.AliasOrDefinedType
import definiti.scalamodel.utils.{ListUtils, StringUtils}

trait AtomicVerificationBuilder {
  self: ScalaModelBuilder =>

  def generatePublicAtomicVerification(aliasOrDefinedType: AliasOrDefinedType): ScalaAST.Statement = {
    val body = ScalaAST.Block(
      generateAtomicInternalVerificationObjects(aliasOrDefinedType) ++
        generateVerification(aliasOrDefinedType)
    ).simplify
    if (aliasOrDefinedType.parameters.nonEmpty || aliasOrDefinedType.genericTypes.nonEmpty) {
      ScalaAST.Def1(
        name = "verification",
        typ = ScalaAST.Type("Verification", generateType(aliasOrDefinedType.internal)),
        generics = aliasOrDefinedType.genericTypes,
        parameters = aliasOrDefinedType.parameters.map(generateParameter),
        body = body,
        property = None
      )
    } else {
      ScalaAST.ClassVal(
        name = "verification",
        typ = ScalaAST.Type("Verification", generateType(aliasOrDefinedType.internal)),
        body = body
      )
    }
  }

  private def generateVerification(aliasOrDefinedType: AliasOrDefinedType): ScalaAST.Statement = {
    val attributeVerifications = aliasOrDefinedType.internal match {
      case definedType: DefinedType => definedType.attributes.flatMap(generateAttributeVerifications(_, definedType))
      case _ => Seq.empty
    }
    val inheritedVerifications = generateInheritedVerifications(aliasOrDefinedType)
    val internalVerifications = generateAtomicInternalVerifications(aliasOrDefinedType)
    val verifications = attributeVerifications ++ inheritedVerifications ++ internalVerifications
    verifications match {
      case Nil => ScalaAST.CallMethod("Verification", "none", generics = generateType(aliasOrDefinedType))
      case list => ScalaAST.CallMethod("Verification", "all", list)
    }
  }

  private def generateAttributeVerifications(attributeDefinition: AttributeDefinition, definedType: DefinedType): Option[ScalaAST.Expression] = {
    val typeVerification = generateTypeVerificationCall(attributeDefinition.typeDeclaration)
    val directVerifications = attributeDefinition.verifications.map(generateVerificationCall(_, definedType))
    val deepVerification = generateDeepVerification(attributeDefinition.typeDeclaration)
    val verifications = typeVerification ++ directVerifications ++ deepVerification
    val groupVerificationOpt = generateGroupVerification(attributeDefinition.typeDeclaration, verifications)
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
        ScalaAST.New(s"ListVerification", Seq.empty, innerVerification)
      } else if (isOption(typeDeclaration)) {
        ScalaAST.New(s"OptionVerification", Seq.empty, innerVerification)
      } else {
        None
      }
    } else {
      None
    }
  }

  private def generateInheritedVerifications(aliasOrDefinedType: AliasOrDefinedType): Seq[ScalaAST.Expression] = {
    verificationsFromType(aliasOrDefinedType.internal)
      .map(generateVerificationCall(_, aliasOrDefinedType))
  }

  private def generateVerificationCall(verificationReference: VerificationReference, aliasOrDefinedType: AliasOrDefinedType): ScalaAST.Expression = {
    val verification = library.verificationsMap(verificationReference.verificationName)
    ScalaAST.New(
      name = verification.fullName,
      generics = replaceGenerics(verification.function.genericTypes, aliasOrDefinedType),
      arguments = verificationReference.parameters.map(generateExpression)
    )
  }

  private def replaceGenerics(generics: Seq[String], aliasOrDefinedType: AliasOrDefinedType): Seq[String] = {
    aliasOrDefinedType.internal match {
      case aliasType: AliasType =>
        aliasType.alias.genericTypes.map(_.readableString)
      case _ =>
        ListUtils.replaceOrdered(generics, aliasOrDefinedType.genericTypes)
    }
  }

  private def generateAtomicInternalVerifications(aliasOrDefinedType: AliasOrDefinedType): Seq[ScalaAST.Expression] = {
    aliasOrDefinedType.verifications
      .collect { case (v: AtomicTypeVerification) => v }
      .indices
      .map { index => ScalaAST.SimpleExpression(s"${aliasOrDefinedType.name}${index}") }
  }

  private def generateAtomicInternalVerificationObjects(aliasOrDefinedType: AliasOrDefinedType): Seq[ScalaAST.Statement] = {
    aliasOrDefinedType.verifications
      .collect { case (v: AtomicTypeVerification) => v }
      .zipWithIndex
      .map { case (typeVerification, index) =>
        generateTypeVerificationObject(s"${aliasOrDefinedType.name}${index}", typeVerification)
      }
  }
}