package definiti.scalamodel.builder

import definiti.core.ast._
import definiti.scalamodel.ScalaAST
import definiti.scalamodel.utils.StringUtils

trait ClassDefinitionBuilder {
  self: ScalaModelBuilder =>

  def generateClassDefinition(classDefinition: ClassDefinition): Seq[ScalaAST.Statement] = classDefinition match {
    case definedType: DefinedType => Seq(generateDefinedType(definedType))
    case aliasType: AliasType => generateAliasType(aliasType)
  }

  private def generateDefinedType(definedType: DefinedType, originalTypeOpt: Option[TypeReference] = None): ScalaAST.Statement = {
    val typeDefinition = s"${generateGenericTypeDefinition(definedType)}"
    val realType = originalTypeOpt.map(_.typeName).getOrElse(definedType.name)
    val originalTypeGenerics = originalTypeOpt match {
      case Some(originalType) => generateGenericTypes(originalType.genericTypes)
      case None => generateGenericTypeDefinition(definedType)
    }
    ScalaAST.StatementsGroup(
      originalTypeOpt match {
        case Some(_) => None
        case None => Some(generateTrait(definedType))
      }
    )
      .plus(definedType.comment.map(ScalaAST.Comment))
      .plus(ScalaAST.ClassDef(
        name = s"${definedType.name}$typeDefinition",
        extendz = Some(StringUtils.prefixOnLastPart(s"$realType$originalTypeGenerics", '.', "$")),
        parameters = generateAttributes(definedType.attributes),
        body = Nil,
        property = None,
        privateConstructor = true
      ))
      .plus(ScalaAST.ObjectDef(
        name = definedType.name,
        body = Seq(
          generateDefinedTypeVerification(definedType),
          generateDefinedTypeApplyFunction(definedType)
        )
      ))
  }

  private def generateDefinedTypeApplyFunction(definedType: DefinedType): ScalaAST.Def1 = {
    val typeDefinition = s"${generateGenericTypeDefinition(definedType)}"
    ScalaAST.Def1(
      name = s"apply$typeDefinition",
      typ = s"Validation[${definedType.name}$typeDefinition]",
      parameters = definedType.attributes.map(generateAttributeParameter),
      body = Some(ScalaAST.Block(
        ScalaAST.Val(
          name = s"built${definedType.name}",
          value = ScalaAST.New(
            name = definedType.name,
            arguments = definedType.attributes.map { attribute =>
              ScalaAST.SimpleExpression(attribute.name)
            }
          )
        ),
        ScalaAST.CallMethod(
          target = typeVerifications(definedType.name),
          name = "verify",
          arguments = Seq(ScalaAST.SimpleExpression(s"built${definedType.name}"))
        )
      ))
    )
  }

  private def typeVerifications(typeName: String): ScalaAST.SimpleExpression = {
    ScalaAST.SimpleExpression(s"${typeName}Verifications")
  }

  private def generateDefinedTypeVerification(definedType: DefinedType): ScalaAST.ClassVal = {
    val typeVerifications = definedType.verifications.map(generateTypeVerification)
    val attributeVerifications = definedType.attributes.flatMap { attribute =>
      attribute.verifications.map { verification =>
        val verificationWithMessage = verification.message match {
          case Some(message) =>
            ScalaAST.CallMethod(
              s"${verification.verificationName}Verification",
              "copy",
              ScalaAST.SimpleExpression(s"""message = "${message}"""")
            )
          case None =>
            ScalaAST.SimpleExpression(s"${verification.verificationName}Verification")
        }
        ScalaAST.CallMethod(
          target = verificationWithMessage,
          name = s"decorate[${definedType.name}]",
          Seq(ScalaAST.SimpleExpression(s"_.${attribute.name}"))
        )
      }
    }
    generateTypeVerifications(definedType, typeVerifications ++ attributeVerifications)
  }

  private def generateTypeVerifications(classDefinition: ClassDefinition, additionalVerifications: Seq[ScalaAST.Expression]): ScalaAST.ClassVal = {
    val inherited = classDefinition match {
      case definedType: DefinedType => definedType.inherited
      case aliasType: AliasType => aliasType.inherited
    }
    val inheritedVerifications = inherited.map { inherited =>
      inherited.message match {
        case Some(message) =>
          ScalaAST.CallMethod(
            s"${inherited.verificationName}Verification",
            "copy",
            ScalaAST.SimpleExpression(s"""message = "$message"""")
          )
        case None =>
          ScalaAST.SimpleExpression(s"${inherited.verificationName}Verification")
      }
    }
    ScalaAST.ClassVal(
      name = s"${classDefinition.name}Verifications",
      typ = s"Verifications[${classDefinition.name}]",
      body = Seq(ScalaAST.CallFunction(
        "Verifications",
        ScalaAST.CallFunction(
          target = ScalaAST.SimpleExpression("Seq"),
          arguments = inheritedVerifications ++ additionalVerifications
        )
      )),
      isLazy = true,
      isPrivate = true
    )
  }

  private def generateTrait(definedType: DefinedType): ScalaAST.TraitDef = {
    ScalaAST.TraitDef(
      s"$$${definedType.name}${generateGenericTypeDefinition(definedType)}",
      definedType.attributes.map { attribute =>
        val attributeType = nativeTypeMapping.getOrElse(attribute.typeReference.typeName, attribute.typeReference.typeName)
        val attributeGenerics = generateGenericTypes(attribute.typeReference.genericTypes)
        ScalaAST.Def0(
          name = attribute.name,
          typ = s"$attributeType$attributeGenerics"
        )
      }
    )
  }

  private def generateAttributes(attributeDefinition: Seq[AttributeDefinition]): Seq[ScalaAST.Parameter] = {
    attributeDefinition.map(generateAttribute)
  }

  private def generateAttribute(attributeDefinition: AttributeDefinition): ScalaAST.Parameter = {
    generateAttributeParameter(attributeDefinition).copy(property = Some("val"))
  }

  private def generateAttributeParameter(attributeDefinition: AttributeDefinition): ScalaAST.Parameter = {
    val attributeType = nativeTypeMapping.getOrElse(attributeDefinition.typeReference.typeName, attributeDefinition.typeReference.typeName)
    val attributeGenerics = generateGenericTypes(attributeDefinition.typeReference.genericTypes)
    ScalaAST.Parameter(attributeDefinition.name, s"$attributeType$attributeGenerics", property = None)
  }

  private def generateTypeVerification(typeVerification: TypeVerification): ScalaAST.CallFunction = {
    // Verification("{message}")(({params}) => {body})
    ScalaAST.CallFunction(
      ScalaAST.CallFunction(
        "Verification",
        ScalaAST.SimpleExpression('"' + typeVerification.message + '"')
      ),
      Seq(generateLambda(typeVerification.function))
    )
  }

  private def generateAliasType(aliasType: AliasType): Seq[ScalaAST.Statement] = {
    library.types(aliasType.alias.typeName) match {
      case definedType: DefinedType => Seq(generateDefinedAliasType(aliasType, definedType))
      case _: NativeClassDefinition => Seq(generateNativeAliasType(aliasType), generateTag(aliasType))
      case _ => throw new RuntimeException("Undefined type: " + aliasType.alias.readableString)
    }
  }

  private def generateDefinedAliasType(aliasType: AliasType, definedType: DefinedType): ScalaAST.Statement = {
    val genericTypeMapping = Map(definedType.genericTypes.zip(aliasType.alias.genericTypes): _*)

    def updateGenericTypes(typeReference: TypeReference): TypeReference = {
      if (genericTypeMapping.contains(typeReference.typeName)) {
        genericTypeMapping(typeReference.typeName)
      } else {
        typeReference.copy(
          genericTypes = typeReference.genericTypes.map(updateGenericTypes)
        )
      }
    }

    generateDefinedType(definedType.copy(
      comment = aliasType.comment,
      name = aliasType.name,
      genericTypes = aliasType.genericTypes,
      inherited = definedType.inherited ++ aliasType.inherited,
      attributes = definedType.attributes.map { attribute =>
        attribute.copy(typeReference = updateGenericTypes(attribute.typeReference))
      }
    ), Some(aliasType.alias))
  }

  private def generateNativeAliasType(aliasType: AliasType): ScalaAST.ObjectDef = {
    ScalaAST.ObjectDef(
      name = aliasType.name,
      body = Seq(
        generateTypeVerifications(aliasType, Seq.empty),
        ScalaAST.Def1(
          name = s"apply",
          typ = s"Validation[${aliasType.name}]",
          parameters = Seq(ScalaAST.Parameter("input", aliasType.alias.typeName, None)),
          body = Some(ScalaAST.Block(
            ScalaAST.CallMethod(
              typeVerifications(aliasType.name),
              "verify",
              Seq(ScalaAST.SimpleExpression(s"input.asInstanceOf[${aliasType.name}]"))
            )
          ))
        )
      )
    )
  }
}
