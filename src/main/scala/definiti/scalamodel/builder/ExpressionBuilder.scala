package definiti.scalamodel.builder

import definiti.common.ast._
import definiti.scalamodel.ScalaAST
import definiti.scalamodel.utils.StringUtils

trait ExpressionBuilder {
  self: ScalaModelBuilder =>

  def generateLambda(definedFunction: DefinedFunction): ScalaAST.Lambda = {
    ScalaAST.Lambda(
      parameters = definedFunction.parameters.map(generateParameter),
      body = generateExpression(definedFunction.body)
    )
  }

  def generateParameter(parameterDefinition: ParameterDefinition): ScalaAST.Parameter = {
    val parameterName = parameterDefinition.name
    val parameterType = generateType(parameterDefinition.typeReference)
    ScalaAST.Parameter(parameterName, parameterType)
  }

  def generateExpression(expression: Expression): ScalaAST.Expression = expression match {
    case boolean: BooleanValue =>
      boolean.value.toString

    case number: NumberValue =>
      ScalaAST.CallFunction("BigDecimal", number.value.toString)

    case quotedString: QuotedStringValue =>
      ScalaAST.StringExpression(quotedString.value.replaceAllLiterally("\\", "\\\\"))

    case reference: Reference =>
      reference.returnType match {
        case NamedFunctionReference(functionName) => StringUtils.lastPart(functionName)
        case _ => reference.name
      }

    case methodCall: MethodCall =>
      generateMethodCall(methodCall)

    case attributeCall: AttributeCall =>
      generateAttributeCall(attributeCall)

    case combinedExpression: CombinedExpression =>
      ScalaAST.Block(combinedExpression.parts.map(generateExpression)).simplify

    case condition: Condition =>
      condition.onFalse
        .map(onFalse => ScalaAST.IfThenElse(generateExpression(condition.condition), generateExpression(condition.onTrue), generateExpression(onFalse)))
        .getOrElse(ScalaAST.IfThen(generateExpression(condition), generateExpression(condition.onTrue)))

    case logicalExpression: LogicalExpression =>
      ScalaAST.BinaryOp(
        generateLogicalOperator(logicalExpression.operator),
        generateExpression(logicalExpression.left),
        generateExpression(logicalExpression.right)
      )

    case calculatorExpression: CalculatorExpression =>
      ScalaAST.BinaryOp(
        generateCalculatorOperator(calculatorExpression.operator),
        generateExpression(calculatorExpression.left),
        generateExpression(calculatorExpression.right)
      )

    case not: Not => ScalaAST.UnaryOp("!", generateExpression(not.inner))

    case functionCall: FunctionCall =>
      ScalaAST.CallFunction(StringUtils.lastPart(functionCall.name), functionCall.parameters.map(generateExpression))

    case lambda: LambdaExpression =>
      ScalaAST.Lambda(lambda.parameterList.map(generateParameter), generateExpression(lambda.expression))

    case _: OkValue =>
      "None"

    case koValue: KoValue =>
      ScalaAST.CallFunction("Some", ScalaAST.CallFunction("Message", ScalaAST.SimpleExpression("message") ++ koValue.parameters.map(generateExpression)))
  }

  private def generateMethodCall(methodCall: MethodCall): ScalaAST.Expression with ScalaAST.Unambiguous with Product with Serializable = {
    methodCall.expression.returnType match {
      case typeReference: TypeReference =>
        getFinalType(typeReference) match {
          case native: NativeClassDefinition =>
            ScalaAST.CallFunction(
              ScalaAST.CallMethod(s"${native.name}Extension", methodCall.method),
              (methodCall.expression ++ methodCall.parameters).map(generateExpression)
            )
          case _ =>
            ScalaAST.CallMethod(generateExpression(methodCall.expression), methodCall.method, methodCall.parameters.map(generateExpression))
        }
      case _: LambdaReference =>
        throw new RuntimeException("cannot call lambda from definedFunction")
      case _: NamedFunctionReference =>
        throw new RuntimeException("cannot call named function from definedFunction")
    }
  }

  private def getFinalType(typeReference: TypeReference): ClassDefinition = {
    library.typesMap(typeReference.typeName) match {
      case aliasType: AliasType => getFinalType(aliasType.alias)
      case other => other
    }
  }

  private def getFinalType(typeDeclaration: TypeDeclaration): ClassDefinition = {
    getFinalType(typeDeclarationToTypeReference(typeDeclaration))
  }

  private def generateAttributeCall(attributeCall: AttributeCall): ScalaAST.Expression = {
    attributeCall.expression.returnType match {
      case typeReference: TypeReference =>
        library.typesMap(typeReference.typeName) match {
          case _: NativeClassDefinition =>
            ScalaAST.CallFunction(
              ScalaAST.CallMethod(s"${typeReference.typeName}Extension", attributeCall.attribute),
              generateExpression(attributeCall.expression)
            )
          case _ =>
            ScalaAST.CallAttribute(generateExpression(attributeCall.expression), attributeCall.attribute)
        }
      case _: LambdaReference =>
        throw new RuntimeException("cannot call lambda from definedFunction")
      case _: NamedFunctionReference =>
        throw new RuntimeException("cannot call named function from definedFunction")
    }
  }

  private def generateLogicalOperator(logicalOperator: LogicalOperator.Value): String = {
    import LogicalOperator._
    logicalOperator match {
      case Or => "||"
      case And => "&&"
      case Equal => "=="
      case NotEqual => "!="
      case Lower => "<"
      case Upper => ">"
      case LowerOrEqual => "<="
      case UpperOrEqual => ">="
    }
  }

  private def generateCalculatorOperator(calculatorOperator: CalculatorOperator.Value): String = {
    import CalculatorOperator._
    calculatorOperator match {
      case Plus => "+"
      case Minus => "-"
      case Modulo => "%"
      case Time => "*"
      case Divide => "/"
    }
  }
}
