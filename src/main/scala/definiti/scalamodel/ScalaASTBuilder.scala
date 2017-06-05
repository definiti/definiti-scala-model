package definiti.scalamodel

import definiti.core._
import definiti.scalamodel.model.PackageFile
import definiti.scalamodel.utils.StringUtils

private[scalamodel] object ScalaASTBuilder {
  val nativeTypeMapping = Map(
    "Number" -> "BigDecimal"
  )

  def build(rootFile: RootFile)(implicit context: Context): String = {
    val generatedCode = ScalaAST.StatementsGroup(
      Seq(
        ScalaAST.PackageDeclaration(rootFile.packageName),
        ScalaAST.Import("definiti.native._"),
        ScalaAST.Import("java.util.Date")
      ) ++ rootFile.classDefinitions.map(generateClassDefinition)
    )

    ScalaCodeGenerator(generatedCode)
  }

  def buildPackageFile(packageFile: PackageFile)(implicit context: Context): String = {
    val packageLine = ScalaAST.PackageDeclaration(StringUtils.excludeLastPart(packageFile.packageName, '.'))
    val imports = ScalaAST.Import("definiti.native._")
    val verifications = packageFile.verifications.map(generateVerification)
    val namedFunction = packageFile.namedFunctions.map(generateNamedFunction)
    val tags = packageFile.nativeAliasType.map(generateTag)
    val generatedCode = ScalaAST.StatementsGroup(
      Seq(packageLine, imports) :+ ScalaAST.PackageDef(
        name = StringUtils.lastPart(packageFile.packageName, '.'),
        body = verifications ++ namedFunction ++ tags
      )
    )
    ScalaCodeGenerator(generatedCode)
  }

  private def generateVerification(verification: Verification)(implicit context: Context): ScalaAST.Statement = {
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
      generics = Seq.empty,
      parameters = verification.function.parameters.map(generateParameter),
      body = Some(ScalaAST.CallMethod(
        target = ScalaAST.SimpleExpression(verificationObjectName),
        name = "verify",
        arguments = verification.function.parameters.map(parameter => ScalaAST.SimpleExpression(parameter.name))
      )),
      property = None
    )
    verification.comment match {
      case Some(comment) =>
        ScalaAST.StatementsGroup(ScalaAST.Comment(comment.trim), verificationObject, verificationFunction)
      case None =>
        ScalaAST.StatementsGroup(verificationObject, verificationFunction)
    }
  }

  private def generateParameter(parameterDefinition: ParameterDefinition)(implicit context: Context): ScalaAST.Parameter = {
    val parameterName = parameterDefinition.name
    val parameterType = generateParameterType(parameterDefinition.typeReference)
    ScalaAST.Parameter(parameterName, parameterType, property = None)
  }

  private def generateParameterType(typeReference: AbstractTypeReference)(implicit context: Context): String = {
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

  private def generateExpression(expression: Expression)(implicit context: Context): ScalaAST.Expression = expression match {
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

  private def generateClassDefinition(classDefinition: ClassDefinition)(implicit context: Context): ScalaAST.Statement = classDefinition match {
    case definedType: DefinedType => generateDefinedType(definedType)
    case aliasType: AliasType => generateAliasType(aliasType)
  }

  private def generateDefinedType(definedType: DefinedType, originalTypeOpt: Option[TypeReference] = None)(implicit context: Context): ScalaAST.Statement = {
    val typeDefinition = s"${generateGenericTypeDefinition(definedType)}"
    val realType = originalTypeOpt.map(_.typeName).getOrElse(definedType.name)
    val originalTypeGenerics = originalTypeOpt match {
      case Some(originalType) => generateGenericTypes(originalType.genericTypes)
      case None => generateGenericTypeDefinition(definedType)
    }
    ScalaAST.StatementsGroup(
      Seq(
        originalTypeOpt match {
          case Some(_) => None
          case None => Some(generateTrait(definedType))
        },
        definedType.comment.map(ScalaAST.Comment),
        Some(ScalaAST.ClassDef(
          name = s"${definedType.name}$typeDefinition",
          extendz = Some(StringUtils.prefixOnLastPart(s"$realType$originalTypeGenerics", '.', "$")),
          parameters = generateAttributes(definedType.attributes),
          body = Nil,
          property = None,
          privateConstructor = true
        )),
        Some(ScalaAST.ObjectDef(
          name = definedType.name,
          body = Seq(
            generateDefinedTypeVerification(definedType),
            ScalaAST.Def1(
              name = s"apply$typeDefinition",
              typ = s"Validation[${definedType.name}$typeDefinition]",
              generics = Seq.empty,
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
                  target = ScalaAST.SimpleExpression(s"${definedType.name}Verifications"),
                  name = "verify",
                  arguments = Seq(ScalaAST.SimpleExpression(s"built${definedType.name}"))
                )
              )),
              property = None
            )
          )
        ))
      ).flatten
    )
  }

  private def generateDefinedTypeVerification(definedType: DefinedType)(implicit context: Context): ScalaAST.ClassVal = {
    generateTypeVerifications(definedType, definedType.verifications.map(generateTypeVerification))
  }

  private def generateNativeAliasType(aliasType: AliasType)(implicit context: Context): ScalaAST.ObjectDef = {
    ScalaAST.ObjectDef(
      name = aliasType.name,
      body = Seq(
        generateTypeVerifications(aliasType, Seq.empty),
        ScalaAST.Def1(
          name = s"apply",
          typ = s"Validation[${aliasType.name}]",
          generics = Seq.empty,
          parameters = Seq(ScalaAST.Parameter("input", aliasType.alias.typeName, None)),
          body = Some(ScalaAST.Block(
            ScalaAST.CallMethod(
              ScalaAST.SimpleExpression(s"${aliasType.name}Verifications"),
              "verify",
              Seq(ScalaAST.SimpleExpression(s"input.asInstanceOf[${aliasType.name}]"))
            )
          )),
          property = None
        )
      )
    )
  }

  private def generateTypeVerifications(aType: Type, additionalVerifications: Seq[ScalaAST.CallFunction])(implicit context: Context): ScalaAST.ClassVal = {
    val inherited = aType match {
      case definedType: DefinedType => definedType.inherited
      case aliasType: AliasType => aliasType.inherited
    }
    val inheritedVerifications = inherited.map { inherited =>
      inherited.message match {
        case Some(message) =>
          ScalaAST.CallMethod(
            target = ScalaAST.SimpleExpression(s"${inherited.verificationName}Verification"),
            name = "copy",
            arguments = Seq(ScalaAST.SimpleExpression(s"message = $message"))
          )
        case None =>
          ScalaAST.SimpleExpression(s"${inherited.verificationName}Verification")
      }
    }
    ScalaAST.ClassVal(
      name = s"${aType.name}Verifications",
      typ = s"Verifications[${aType.name}]",
      body = Seq(ScalaAST.CallFunction(
        target = ScalaAST.SimpleExpression("Verifications"),
        arguments = Seq(ScalaAST.CallFunction(
          target = ScalaAST.SimpleExpression("Seq"),
          arguments = inheritedVerifications ++ additionalVerifications
        ))
      )),
      isLazy = true,
      isPrivate = true
    )
  }

  private def generateTrait(definedType: DefinedType)(implicit context: Context): ScalaAST.TraitDef = {
    //
    // trait $${definedType}${generateGenericTypeDefinition(definedType)} {
    //   ${definedType.attributes.map(attribute => "def " + generateAttributeParameter_(attribute)).mkString("\n")}
    // }
    ScalaAST.TraitDef(
      s"$$${definedType.name}${generateGenericTypeDefinition(definedType)}",
      definedType.attributes.map { attribute =>
        val attributeType = nativeTypeMapping.getOrElse(attribute.typeReference.typeName, attribute.typeReference.typeName)
        val attributeGenerics = generateGenericTypes(attribute.typeReference.genericTypes)
        ScalaAST.Def0(
          name = attribute.name,
          typ = s"$attributeType$attributeGenerics",
          generics = Seq.empty,
          body = None,
          property = None
        )
      }
    )
  }

  private def generateGenericTypeDefinition(definedType: DefinedType) = {
    if (definedType.genericTypes.nonEmpty) {
      definedType.genericTypes.mkString("[", ",", "]")
    } else {
      ""
    }
  }

  private def generateAliasType(aliasType: AliasType)(implicit context: Context): ScalaAST.Statement = {
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


  private def generateAttributes(attributeDefinition: Seq[AttributeDefinition])(implicit context: Context): Seq[ScalaAST.Parameter] = {
    attributeDefinition.map(generateAttribute)
  }

  private def generateAttribute(attributeDefinition: AttributeDefinition)(implicit context: Context): ScalaAST.Parameter = {
    generateAttributeParameter(attributeDefinition).copy(property = Some("val"))
  }

  private def generateAttributeParameter(attributeDefinition: AttributeDefinition)(implicit context: Context): ScalaAST.Parameter = {
    val attributeType = nativeTypeMapping.getOrElse(attributeDefinition.typeReference.typeName, attributeDefinition.typeReference.typeName)
    val attributeGenerics = generateGenericTypes(attributeDefinition.typeReference.genericTypes)
    ScalaAST.Parameter(attributeDefinition.name, s"$attributeType$attributeGenerics", property = None)
  }

  private def generateTypeVerification(typeVerification: TypeVerification)(implicit context: Context): ScalaAST.CallFunction = {
    // Verification("{message}")(({params}) => {body})
    ScalaAST.CallFunction(
      ScalaAST.CallFunction(
        ScalaAST.SimpleExpression("Verification"),
        Seq(ScalaAST.SimpleExpression('"' + typeVerification.message + '"'))
      ),
      Seq(generateLambda(typeVerification.function))
    )
  }

  private def generateGenericTypes(genericTypes: Seq[TypeReference]): String = {
    def generateGenericType(genericType: TypeReference): String = {
      nativeTypeMapping.getOrElse(genericType.typeName, genericType.typeName) + generateGenericTypes(genericType.genericTypes)
    }
    if (genericTypes.nonEmpty) {
      genericTypes.map(generateGenericType).mkString("[", ",", "]")
    } else {
      ""
    }
  }

  private def generateNamedFunction(namedFunction: NamedFunction)(implicit context: Context): ScalaAST.Def1 = {
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
      body = Some(generateExpression(namedFunction.body)(namedFunctionContext)),
      property = None
    )
  }

  private def generateElementReference(elementReference: ElementReference)(implicit context: Context): String = {
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

  private def generateLambda(definedFunction: DefinedFunction)(implicit context: Context): ScalaAST.Lambda = {
    val lambdaContext = DefinedFunctionContext(context, definedFunction)
    ScalaAST.Lambda(
      parameters = definedFunction.parameters.map(generateParameter),
      body = generateExpression(definedFunction.body)(lambdaContext)
    )
  }

  private def generateTag(aliasType: AliasType)(implicit context: Context): ScalaAST.Statement = {
    ScalaAST.StatementsGroup(
      ScalaAST.TraitDef(s"${aliasType.name}Tag", Seq.empty, isSealed = true),
      ScalaAST.TypeDef(aliasType.name, s"${generateParameterType(aliasType.alias)} @@ ${aliasType.name}Tag")
    )
  }
}
