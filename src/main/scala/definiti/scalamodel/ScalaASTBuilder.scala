package definiti.scalamodel

import definiti.core._
import definiti.scalamodel.model.PackageFile
import definiti.scalamodel.utils.StringUtils

private[scalamodel] object ScalaASTBuilder {
  val nativeTypeMapping = Map(
    "Number" -> "BigDecimal"
  )

  def build(rootFile: RootFile)(implicit context: Context): String = {
    val generatedCode = ScalaAST.StatementsGroup(ScalaAST.PackageDeclaration(rootFile.packageName))
      .plus(importLines)
      .plus(rootFile.classDefinitions.map(generateClassDefinition))

    ScalaCodeGenerator(generatedCode)
  }

  def buildPackageFile(packageFile: PackageFile)(implicit context: Context): String = {
    val packageLine = ScalaAST.PackageDeclaration(StringUtils.excludeLastPart(packageFile.packageName, '.'))
    val verifications = packageFile.verifications.map(generateVerification)
    val namedFunction = packageFile.namedFunctions.map(generateNamedFunction)
    val tags = packageFile.nativeAliasType.map(generateTag)

    val generatedCode = ScalaAST.StatementsGroup(packageLine)
      .plus(importLines)
      .plus(ScalaAST.PackageDef(
        name = StringUtils.lastPart(packageFile.packageName, '.'),
        body = verifications ++ namedFunction ++ tags
      ))

    ScalaCodeGenerator(generatedCode)
  }

  def importLines: ScalaAST.StatementsGroup = {
    ScalaAST.StatementsGroup(
      ScalaAST.Import("definiti.native._"),
      ScalaAST.Import("java.util.Date")
    )
  }

  def generateVerification(verification: Verification)(implicit context: Context): ScalaAST.Statement = {
    val verificationObjectName = s"${verification.name}Verification"
    val verificationFunctionName = s"verify${verification.name}"
    val verificationObject = ScalaAST.Val(
      name = verificationObjectName,
      value = ScalaAST.CallFunction(
        ScalaAST.CallFunction(
          ScalaAST.SimpleExpression("Verification"),
          Seq(ScalaAST.SimpleExpression('"' + verification.message + '"'))
        ),
        Seq(generateLambda(verification.function))
      ),
      isLazy = true
    )
    val verificationFunction = ScalaAST.Def1(
      name = verificationFunctionName,
      typ = s"Validation[${generateParameterType(verification.function.parameters.head.typeReference)}]",
      parameters = verification.function.parameters.map(generateParameter),
      body = Some(ScalaAST.CallMethod(
        target = ScalaAST.SimpleExpression(verificationObjectName),
        name = "verify",
        arguments = verification.function.parameters.map(parameter => ScalaAST.SimpleExpression(parameter.name))
      ))
    )
    ScalaAST.StatementsGroup(verification.comment.map(comment => ScalaAST.Comment(comment.trim)))
      .plus(verificationObject)
      .plus(verificationFunction)
  }

  def generateParameter(parameterDefinition: ParameterDefinition)(implicit context: Context): ScalaAST.Parameter = {
    val parameterName = parameterDefinition.name
    val parameterType = generateParameterType(parameterDefinition.typeReference)
    ScalaAST.Parameter(parameterName, parameterType, property = None)
  }

  def generateParameterType(typeReference: AbstractTypeReference)(implicit context: Context): String = {
    typeReference match {
      case TypeReference(typeName, genericTypes) =>
        val finalTypeName = context.findType(typeName) match {
          case Some(_: Type) => StringUtils.prefixOnLastPart(typeName, '.', "$")
          case _ => nativeTypeMapping.getOrElse(typeName, typeName)
        }
        val parameterGenerics = generateGenericTypes(genericTypes)
        finalTypeName + parameterGenerics
      case LambdaReference(inputTypes, outputType) =>
        def generateOneType(typeReference: TypeReference): String = {
          val typeName = typeReference.typeName
          val generics = generateGenericTypes(typeReference.genericTypes)
          typeName + generics
        }
        s"(${inputTypes.map(generateOneType)}) => ${generateOneType(outputType)}"
    }
  }

  def generateExpression(expression: Expression)(implicit context: Context): ScalaAST.Expression = expression match {
    case BooleanValue(value, _) =>
      ScalaAST.SimpleExpression(value.toString)
    case NumberValue(value, _) =>
      ScalaAST.SimpleExpression(value.toString)
    case QuotedStringValue(value, _) =>
      ScalaAST.SimpleExpression('"' + value.replaceAllLiterally("\\", "\\\\") + '"')
    case Reference(variable, _) =>
      ScalaAST.SimpleExpression(variable)
    case MethodCall(inner, method, parameters, _, _) =>
      ASTHelper.getReturnTypeOfExpression(inner) match {
        case ClassReference(classDefinition: NativeClassDefinition, _) =>
          ScalaAST.CallFunction(
            ScalaAST.SimpleExpression(s"${classDefinition.name}Extension.$method"),
            (inner +: parameters).map(generateExpression)
          )
        case ClassReference(_, _) =>
          ScalaAST.CallMethod(generateExpression(inner), method, parameters.map(generateExpression))
        case NamedFunctionReference(_) =>
          throw new RuntimeException("cannot call method from definedFunction")
      }
    case AttributeCall(inner, attribute, _) =>
      ASTHelper.getReturnTypeOfExpression(inner) match {
        case ClassReference(classDefinition: NativeClassDefinition, _) =>
          ScalaAST.CallFunction(
            ScalaAST.SimpleExpression(s"${classDefinition.name}Extension.$attribute"),
            Seq(generateExpression(inner))
          )
        case ClassReference(_, _) =>
          ScalaAST.CallAttribute(generateExpression(inner), attribute)
        case NamedFunctionReference(_) =>
          throw new RuntimeException("cannot call method from definedFunction")
      }
    case CombinedExpression(expressions, _) =>
      ScalaAST.Block(expressions.map(generateExpression))
    case Condition(condition, onTrue, Some(onFalse), _) =>
      ScalaAST.IfThenElse (generateExpression(condition), generateExpression(onTrue), generateExpression(onFalse))
    case Condition(condition, onTrue, None, _) =>
      ScalaAST.IfThen(generateExpression(condition), generateExpression(onTrue))
    case Or(left, right, _) => ScalaAST.BinaryOp("||", generateExpression(left), generateExpression(right))
    case And(left, right, _) => ScalaAST.BinaryOp("&&", generateExpression(left), generateExpression(right))
    case Equal(left, right, _) => ScalaAST.BinaryOp("==", generateExpression(left), generateExpression(right))
    case NotEqual(left, right, _) => ScalaAST.BinaryOp("!=", generateExpression(left), generateExpression(right))
    case Lower(left, right, _) => ScalaAST.BinaryOp("<", generateExpression(left), generateExpression(right))
    case Upper(left, right, _) => ScalaAST.BinaryOp(">", generateExpression(left), generateExpression(right))
    case LowerOrEqual(left, right, _) => ScalaAST.BinaryOp("<=", generateExpression(left), generateExpression(right))
    case UpperOrEqual(left, right, _) => ScalaAST.BinaryOp(">=", generateExpression(left), generateExpression(right))
    case Plus(left, right, _) => ScalaAST.BinaryOp("+", generateExpression(left), generateExpression(right))
    case Minus(left, right, _) => ScalaAST.BinaryOp("-", generateExpression(left), generateExpression(right))
    case Modulo(left, right, _) => ScalaAST.BinaryOp("%", generateExpression(left), generateExpression(right))
    case Time(left, right, _) => ScalaAST.BinaryOp("*", generateExpression(left), generateExpression(right))
    case Divide(left, right, _) => ScalaAST.BinaryOp("/", generateExpression(left), generateExpression(right))
    case Not(inner, _) => ScalaAST.UnaryOp("!", generateExpression(inner))
    case FunctionCall(name, parameters, _, _) =>
      ScalaAST.CallFunction(ScalaAST.SimpleExpression(name), parameters.map(generateExpression))
    case LambdaExpression(parameterList, inner, _) =>
      ScalaAST.Lambda(parameterList.map(generateParameter), generateExpression(inner))
  }

  def generateClassDefinition(classDefinition: ClassDefinition)(implicit context: Context): ScalaAST.Statement = classDefinition match {
    case definedType: DefinedType => generateDefinedType(definedType)
    case aliasType: AliasType => generateAliasType(aliasType)
  }

  def generateDefinedType(definedType: DefinedType, originalTypeOpt: Option[TypeReference] = None)(implicit context: Context): ScalaAST.Statement = {
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

  private def generateDefinedTypeApplyFunction(definedType: DefinedType)(implicit context: Context) = {
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

  def typeVerifications(typeName: String): ScalaAST.SimpleExpression = {
    ScalaAST.SimpleExpression(s"${typeName}Verifications")
  }

  def generateDefinedTypeVerification(definedType: DefinedType)(implicit context: Context): ScalaAST.ClassVal = {
    val typeVerifications = definedType.verifications.map(generateTypeVerification)
    val attributeVerifications = definedType.attributes.flatMap { attribute =>
      attribute.verifications.map { verification =>
        val verificationWithMessage = verification.message match {
          case Some(message) =>
            ScalaAST.CallMethod(
              s"${verification.verificationName}Verification",
              "copy",
              ScalaAST.SimpleExpression(s"message = $message")
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

  def generateNativeAliasType(aliasType: AliasType)(implicit context: Context): ScalaAST.ObjectDef = {
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

  def generateTypeVerifications(aType: Type, additionalVerifications: Seq[ScalaAST.Expression])(implicit context: Context): ScalaAST.ClassVal = {
    val inherited = aType match {
      case definedType: DefinedType => definedType.inherited
      case aliasType: AliasType => aliasType.inherited
    }
    val inheritedVerifications = inherited.map { inherited =>
      inherited.message match {
        case Some(message) =>
          ScalaAST.CallMethod(
            s"${inherited.verificationName}Verification",
            "copy",
            ScalaAST.SimpleExpression(s"message = $message")
          )
        case None =>
          ScalaAST.SimpleExpression(s"${inherited.verificationName}Verification")
      }
    }
    ScalaAST.ClassVal(
      name = s"${aType.name}Verifications",
      typ = s"Verifications[${aType.name}]",
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

  def generateTrait(definedType: DefinedType)(implicit context: Context): ScalaAST.TraitDef = {
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

  def generateAliasType(aliasType: AliasType)(implicit context: Context): ScalaAST.Statement = {
    context.findType(aliasType.alias.typeName) match {
      case Some(definedType: DefinedType) =>
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
      case Some(_: NativeClassDefinition) =>
        generateNativeAliasType(aliasType)
      case _ =>
        throw new RuntimeException("Undefined type: " + aliasType.alias.typeName)
    }
  }


  def generateAttributes(attributeDefinition: Seq[AttributeDefinition])(implicit context: Context): Seq[ScalaAST.Parameter] = {
    attributeDefinition.map(generateAttribute)
  }

  def generateAttribute(attributeDefinition: AttributeDefinition)(implicit context: Context): ScalaAST.Parameter = {
    generateAttributeParameter(attributeDefinition).copy(property = Some("val"))
  }

  def generateAttributeParameter(attributeDefinition: AttributeDefinition)(implicit context: Context): ScalaAST.Parameter = {
    val attributeType = nativeTypeMapping.getOrElse(attributeDefinition.typeReference.typeName, attributeDefinition.typeReference.typeName)
    val attributeGenerics = generateGenericTypes(attributeDefinition.typeReference.genericTypes)
    ScalaAST.Parameter(attributeDefinition.name, s"$attributeType$attributeGenerics", property = None)
  }

  def generateTypeVerification(typeVerification: TypeVerification)(implicit context: Context): ScalaAST.CallFunction = {
    // Verification("{message}")(({params}) => {body})
    ScalaAST.CallFunction(
      ScalaAST.CallFunction(
        "Verification",
        ScalaAST.SimpleExpression('"' + typeVerification.message + '"')
      ),
      Seq(generateLambda(typeVerification.function))
    )
  }

  def generateGenericTypeDefinition(definedType: DefinedType): String = {
    if (definedType.genericTypes.nonEmpty) {
      definedType.genericTypes.mkString("[", ",", "]")
    } else {
      ""
    }
  }

  def generateGenericTypes(genericTypes: Seq[TypeReference]): String = {
    def generateGenericType(genericType: TypeReference): String = {
      nativeTypeMapping.getOrElse(genericType.typeName, genericType.typeName) + generateGenericTypes(genericType.genericTypes)
    }
    if (genericTypes.nonEmpty) {
      genericTypes.map(generateGenericType).mkString("[", ",", "]")
    } else {
      ""
    }
  }

  def generateNamedFunction(namedFunction: NamedFunction)(implicit context: Context): ScalaAST.Def1 = {
    val namedFunctionContext = DefinedFunctionContext(
      outerContext = context,
      currentFunction = namedFunction.function
    )
    ScalaAST.Def1(
      name = namedFunction.name,
      typ = generateElementReference(ASTHelper.getReturnTypeOfExpression(namedFunction.function.body)(namedFunctionContext)),
      generics = namedFunction.function.genericTypes,
      parameters = namedFunction.parameters.map { parameterDefinition =>
        ScalaAST.Parameter(
          name = parameterDefinition.name,
          typ = generateParameterType(parameterDefinition.typeReference),
          property = None
        )
      },
      body = Some(generateExpression(namedFunction.body)(namedFunctionContext))
    )
  }

  def generateElementReference(elementReference: ElementReference)(implicit context: Context): String = {
    def classReferenceToTypeReference(classReference: ClassReference): TypeReference = {
      TypeReference(
        typeName = classReference.classDefinition.canonicalName,
        genericTypes = classReference.genericTypes.map(classReferenceToTypeReference)
      )
    }

    elementReference match {
      case classReference: ClassReference =>
        generateParameterType(classReferenceToTypeReference(classReference))
      case _ =>
        throw new RuntimeException("Return type of anything other than classReference in expression not accepted")
    }
  }

  def generateLambda(definedFunction: DefinedFunction)(implicit context: Context): ScalaAST.Lambda = {
    val lambdaContext = DefinedFunctionContext(context, definedFunction)
    ScalaAST.Lambda(
      parameters = definedFunction.parameters.map(generateParameter),
      body = generateExpression(definedFunction.body)(lambdaContext)
    )
  }

  def generateTag(aliasType: AliasType)(implicit context: Context): ScalaAST.Statement = {
    ScalaAST.StatementsGroup(
      ScalaAST.TraitDef(s"${aliasType.name}Tag", Seq.empty, isSealed = true),
      ScalaAST.TypeDef(aliasType.name, s"${generateParameterType(aliasType.alias)} @@ ${aliasType.name}Tag")
    )
  }
}
