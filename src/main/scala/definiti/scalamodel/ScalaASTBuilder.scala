package definiti.scalamodel

import definiti._
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
    val buffer: StringBuilder = new StringBuilder()

    buffer.append(
      """/* ***************************************************************** *
        | * Native
        | * ***************************************************************** */
        |""".stripMargin)

    appendNative(buffer)

    buffer.append(ScalaCodeGenerator(ScalaAST.StatementsGroup(Seq(
      ScalaAST.Comment("""***************************************************************** *
                         | * Verification definitions
                         | * *****************************************************************""".stripMargin),
      ScalaAST.PackageDef(
        "object verifications",
        root.verifications.map(generateVerification) :+ ScalaAST.Def2(
          name = "verify",
          typ = "Option[String]",
          parameters1 = Seq(ScalaAST.Parameter("message", "String")),
          parameters2 = Seq(ScalaAST.Parameter("condition", "=> Boolean")),
          body = ScalaAST.IfThenElse(
            ScalaAST.SimpleExpression("condition"),
            ScalaAST.SimpleExpression("None"),
            ScalaAST.SimpleExpression("Some(message)")
          ),
          property = Some("private")
        )
      ),
      ScalaAST.Import("verifications._"),
      ScalaAST.Comment("""***************************************************************** *
                         | * Class definitions
                         | * *****************************************************************""".stripMargin)
    ))))

    buffer.append(root.classDefinitions.map(generateClassDefinition).mkString("\n\n"))

    buffer.toString()
  }

  private def appendNative(buffer: StringBuilder)(implicit context: Context): Unit = {
    Seq("BooleanWrapper", "DateWrapper", "ListWrapper", "NumberWrapper", "StringWrapper") foreach { className =>
      buffer.append(Source.fromResource(s"native/$className.scala").getLines.mkString("", "\n", "\n"))
    }
  }

  private def generateVerification(verification: Verification)(implicit context: Context): ScalaAST.Statement = {
    // // comments
    // def verify${name}(${verification.function.parameters}): Option[String] = {
    //   verify("${verification.message}") {
    //     ${verification.function.body}
    //   }
    // }
    val body = ScalaAST.CallFunction(
      ScalaAST.CallFunction(
        ScalaAST.SimpleExpression("verify"),
        Seq(ScalaAST.SimpleExpression(s""""${verification.message}""""))
      ),
      Seq(ScalaAST.Block(Seq(generateExpression(verification.function.body))))
    )
    val func = ScalaAST.Def(
      s"verify${verification.name}",
      "Option[String]",
      verification.function.parameters.map(generateParameter),
      body,
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
    ScalaAST.Parameter(parameterName, parameterType)
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
      ScalaAST.SimpleExpression(s"new BooleanWrapper(${value.toString})")
    case NumberValue(value, _) =>
      ScalaAST.SimpleExpression(s"new NumberWrapper(${value.toString})")
    case QuotedStringValue(value, _) =>
      ScalaAST.SimpleExpression(s"""new StringWrapper("${value.toString.replaceAllLiterally("\\", "\\\\")}")""")
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
    case Lower(left, right, _) => ScalaAST.BinaryOp("< ", generateExpression(left), generateExpression(right))
    case Upper(left, right, _) => ScalaAST.BinaryOp("> ", generateExpression(left), generateExpression(right))
    case LowerOrEqual(left, right, _) => ScalaAST.BinaryOp("<=", generateExpression(left), generateExpression(right))
    case UpperOrEqual(left, right, _) => ScalaAST.BinaryOp(">=", generateExpression(left), generateExpression(right))
    case Plus(left, right, _) => ScalaAST.BinaryOp("+ ", generateExpression(left), generateExpression(right))
    case Minus(left, right, _) => ScalaAST.BinaryOp("- ", generateExpression(left), generateExpression(right))
    case Modulo(left, right, _) => ScalaAST.BinaryOp("% ", generateExpression(left), generateExpression(right))
    case Time(left, right, _) => ScalaAST.BinaryOp("* ", generateExpression(left), generateExpression(right))
    case Divide(left, right, _) => ScalaAST.BinaryOp("/ ", generateExpression(left), generateExpression(right))
    case Not(inner, _) => ScalaAST.UnaryOp("!", generateExpression(inner))
    case LambdaExpression(parameterList, inner, _) =>
      ScalaAST.Lambda(parameterList.map(generateParameter), generateExpression(inner))
  }

  private def generateClassDefinition(classDefinition: ClassDefinition)(implicit context: Context): String = classDefinition match {
    case definedType: DefinedType => generateDefinedType(definedType)
    case aliasType: AliasType => generateAliasType(aliasType)
  }

  private def generateDefinedType(definedType: DefinedType, originalTypeOpt: Option[TypeReference] = None)(implicit context: Context): String = {
    val __resultAliases = definedType.verifications
      .flatMap(_.function.parameters.map(_.name))
      .distinct
      .map(parameter => s"val $parameter = __result")
      .mkString("\n")

    val verifications = (
      definedType.verifications.map(generateTypeVerification)
        ++
        definedType.inherited.map(inherited => s"verify$inherited(__result)")
      ).mkString(", ")

    val realType = originalTypeOpt.map(_.typeName).getOrElse(definedType.name)

    val resultType = originalTypeOpt match {
      case Some(_) =>
        s"new ${definedType.name}(${definedType.attributes.map(attribute => s"__result.${attribute.name}").mkString(", ")})"
      case None =>
        "__result"
    }

    val $interface = originalTypeOpt.map(_ => "").getOrElse(generateInterface(definedType))

    val originalTypeGenerics = originalTypeOpt match {
      case Some(originalType) => generateGenericTypes(originalType.genericTypes)
      case None => generateGenericTypeDefinition(definedType)
    }

    val typeDefinition = s"${generateGenericTypeDefinition(definedType)}"
    s"""
       |${$interface}
       |${definedType.comment.map(comment => s"/*$comment*/").getOrElse("")}
       |class ${definedType.name}$typeDefinition private(${generateAttributes(definedType.attributes)}) extends $$$realType$originalTypeGenerics
       |
       |object ${definedType.name} {
       |  def apply$typeDefinition(${generateAttributeParameters(definedType.attributes)}): Either[String, ${definedType.name}$typeDefinition] = {
       |    val __result = new ${definedType.name}(${definedType.attributes.map(_.name).mkString(", ")})
       |    ${__resultAliases}
       |    val __errorOpt = Seq(
       |      $verifications
       |    ).find(_.isDefined).flatten
       |
       |    __errorOpt match {
       |      case Some(error) => Left(error)
       |      case None =>
       |
       |      Right($resultType)
       |    }
       |  }
       |
       |  private def verify(message: String)(condition: => Boolean) = {
       |    if (condition) {
       |      None
       |    } else {
       |      Some(message)
       |    }
       |  }
       |}
     """.stripMargin
  }

  private def generateInterface(definedType: DefinedType)(implicit context: Context): String = {
    s"""
       |trait $$${definedType.name}${generateGenericTypeDefinition(definedType)} {
       |  ${definedType.attributes.map(attribute => "def " + generateAttributeParameter(attribute)).mkString("\n")}
       |}
     """.stripMargin
  }

  private def generateGenericTypeDefinition(definedType: DefinedType) = {
    if (definedType.genericTypes.nonEmpty) {
      definedType.genericTypes.mkString("[", ",", "]")
    } else {
      ""
    }
  }

  private def generateAliasType(aliasType: AliasType)(implicit context: Context): String = {
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

  private def generateAttributes(attributeDefinition: Seq[AttributeDefinition])(implicit context: Context): String = {
    attributeDefinition.map(generateAttribute).mkString(", ")
  }

  private def generateAttribute(attributeDefinition: AttributeDefinition)(implicit context: Context): String = {
    s"val ${generateAttributeParameter(attributeDefinition)}"
  }

  private def generateAttributeParameters(attributeDefinition: Seq[AttributeDefinition])(implicit context: Context): String = {
    attributeDefinition.map(generateAttributeParameter).mkString(", ")
  }

  private def generateAttributeParameter(attributeDefinition: AttributeDefinition)(implicit context: Context): String = {
    val attributeName = attributeDefinition.name
    val attributeType = nativeTypeMapping.getOrElse(attributeDefinition.typeReference.typeName, attributeDefinition.typeReference.typeName)
    val attributeGenerics = generateGenericTypes(attributeDefinition.typeReference.genericTypes)
    s"$attributeName: $attributeType$attributeGenerics"
  }

  private def generateTypeVerification(typeVerification: TypeVerification)(implicit context: Context): String = {
    s"""
       |verify("${typeVerification.message}") {
       |  ${generateExpression(typeVerification.function.body)}
       |}
     """.stripMargin
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
