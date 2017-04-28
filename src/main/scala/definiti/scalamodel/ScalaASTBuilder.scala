package definiti.scalamodel

import definiti.{scalamodel, _}
import definiti.api.Context

import scala.io.Source

object ScalaASTBuilder {
  val nativeTypeMapping = Map(
    "Boolean" -> "BooleanWrapper",
    "Date" -> "DateWrapper",
    "List" -> "ListWrapper",
    "Number" -> "NumberWrapper",
    "String" -> "StringWrapper"
  )

  def build(root: Root)(implicit context: Context): String = {
    val generatedCode = ScalaAST.StatementsGroup(
      Seq(
        ScalaAST.Comment("""***************************************************************** *
                           | * Verification definitions
                           | * *****************************************************************""".stripMargin),
        ScalaAST.PackageDef(
          "object verifications",
          root.verifications.map(generateVerification) :+ generateGenericVerifyFunction()
        ),
        ScalaAST.Import("verifications._"),
        ScalaAST.Comment("""***************************************************************** *
                           | * Class definitions
                           | * *****************************************************************""".stripMargin)
      ) ++ root.classDefinitions.map(generateClassDefinition)
    )

    val buffer: StringBuilder = new StringBuilder()
    buffer.append(
      """/* ***************************************************************** *
        | * Native
        | * ***************************************************************** */
        |""".stripMargin)
    appendNative(buffer)
    buffer.append(ScalaCodeGenerator(generatedCode))
    buffer.toString()
  }

  private def appendNative(buffer: StringBuilder)(implicit context: Context): Unit = {
    Seq("BooleanWrapper", "DateWrapper", "ListWrapper", "NumberWrapper", "StringWrapper") foreach { className =>
      buffer.append(Source.fromResource(s"native/$className.scala").getLines.mkString("", "\n", "\n"))
    }
  }

  private def generateGenericVerifyFunction(): ScalaAST.Def2 = {
    ScalaAST.Def2(
      name = "verify",
      typ = "Option[String]",
      parameters1 = Seq(ScalaAST.Parameter("message", "String", property = None)),
      parameters2 = Seq(ScalaAST.Parameter("condition", "=> Boolean", property = None)),
      body = Some(ScalaAST.IfThenElse(
        ScalaAST.SimpleExpression("condition"),
        ScalaAST.SimpleExpression("None"),
        ScalaAST.SimpleExpression("Some(message)")
      )),
      property = Some("private")
    )
  }

  private def generateVerification(verification: Verification)(implicit context: Context): ScalaAST.Statement = {
    val body = ScalaAST.CallFunction(
      ScalaAST.CallFunction(
        ScalaAST.SimpleExpression("verify"),
        Seq(ScalaAST.SimpleExpression('"' + verification.message + '"'))
      ),
      Seq(ScalaAST.Block(Seq(generateExpression(verification.function.body))))
    )
    val func = ScalaAST.Def1(
      name = s"verify${verification.name}",
      typ = "Option[String]",
      parameters = verification.function.parameters.map(generateParameter),
      body = Some(body),
      property = None
    )
    verification.comment match {
      case Some(comment) => ScalaAST.StatementsGroup(Seq(ScalaAST.Comment(comment.trim), func))
      case None => func
    }
  }

  private def generateParametersWithoutExternalBraces(parameterDefinitions: Seq[ParameterDefinition])(implicit context: Context): String = parameterDefinitions match {
    case Nil => ""
    case seq => seq.map(generateParameter).mkString(",")
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
          case Some(_: Type) => "$" + typeName
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
      ScalaAST.New("BooleanWrapper", Seq(ScalaAST.SimpleExpression(value.toString)))
    case NumberValue(value, _) =>
      ScalaAST.New("NumberWrapper", Seq(ScalaAST.SimpleExpression(value.toString)))
    case QuotedStringValue(value, _) =>
      ScalaAST.New("StringWrapper", Seq(ScalaAST.SimpleExpression('"' + value.replaceAllLiterally("\\", "\\\\") + '"')))
    case Variable(variable, _, _) =>
      ScalaAST.SimpleExpression(variable)
    case MethodCall(inner, method, parameters, _, _) =>
      ScalaAST.CallMethod(generateExpression(inner), method, parameters.map(generateExpression))
    case AttributeCall(inner, attribute, _) =>
      ScalaAST.CallAttribute(generateExpression(inner), attribute)
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
          extendz = Some(s"$$$realType$originalTypeGenerics"),
          parameters = generateAttributes(definedType.attributes),
          body = Nil,
          property = None,
          privateConstructor = true
        )),
        Some(ScalaAST.ObjectDef(
          name = definedType.name,
          body = Seq(
            ScalaAST.Def1(
              name = s"apply$typeDefinition",
              typ = s"Either[String, ${definedType.name}$typeDefinition]",
              parameters = definedType.attributes.map(generateAttributeParameter),
              body = Some(ScalaAST.Block(Seq(
                ScalaAST.Val(
                  name = "__result",
                  value = ScalaAST.New(
                    name = definedType.name,
                    arguments = definedType.attributes.map { attribute =>
                      ScalaAST.SimpleExpression(attribute.name)
                    }
                  )
                ),
                ScalaAST.StatementsGroup(
                  definedType.verifications
                    .flatMap(_.function.parameters.map(_.name))
                    .distinct
                    .map(parameter => ScalaAST.Val(parameter, ScalaAST.SimpleExpression("__result")))
                ),
                ScalaAST.Val("__errorOpt", ScalaAST.CallAttribute(
                  ScalaAST.CallMethod(
                    ScalaAST.CallFunction(
                      ScalaAST.SimpleExpression("Seq"),
                      definedType.verifications.map(generateTypeVerification) ++
                        definedType.inherited.map { inherited =>
                          ScalaAST.CallFunction(ScalaAST.SimpleExpression(s"verify$inherited"), Seq(ScalaAST.SimpleExpression("__result")))
                        }
                    ),
                    "find",
                    Seq(ScalaAST.CallAttribute(
                      ScalaAST.SimpleExpression("_"),
                      "isDefined"
                    ))
                  ),
                  "flatten"
                )),
                {
                  ScalaAST.Match(
                    expr = ScalaAST.SimpleExpression("__errorOpt"),
                    cases = Seq(
                      ScalaAST.Case(pattern = "Some(error)", body = ScalaAST.CallFunction(ScalaAST.SimpleExpression("Left"), Seq(
                        ScalaAST.SimpleExpression("error")
                      ))),
                      ScalaAST.Case(pattern = "None", body = ScalaAST.CallFunction(ScalaAST.SimpleExpression("Right"), Seq(
                        originalTypeOpt match {
                          case Some(_) =>
                            ScalaAST.New(definedType.name, definedType.attributes.map { attribute =>
                              ScalaAST.CallAttribute(ScalaAST.SimpleExpression("__result"), attribute.name)
                            })
                          case None => ScalaAST.SimpleExpression("__result")
                        }
                      )))
                    )
                  )
                }
              ))),
              property = None
            ),
            generateGenericVerifyFunction()
          )
        ))
      ).flatten
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
      case _ => throw new RuntimeException("Undefined type: " + aliasType)
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
    // verify("{message}") {
    //   {body}
    // }
    ScalaAST.CallFunction(
      ScalaAST.CallFunction(
        ScalaAST.SimpleExpression("verify"),
        Seq(ScalaAST.SimpleExpression('"' + typeVerification.message + '"'))
      ),
      Seq(generateExpression(typeVerification.function.body))
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
}
