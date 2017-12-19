package definiti.scalamodel.builder

import definiti.core.ast._
import definiti.scalamodel.ScalaAST
import definiti.scalamodel.utils.StringUtils

trait ImportExtractor {
  self: ScalaModelBuilder =>

  protected def extractImportsFromNamespace(namespace: Namespace): Seq[ScalaAST.Import] = {
    namespace.elements
      .view
      .flatMap {
        case verification: Verification => extractFromVerification(verification)
        case namedFunction: NamedFunction => extractFromNamedFunction(namedFunction)
        case classDefinition: ClassDefinition => extractFromClassDefinition(classDefinition)
        case _ => Seq.empty
      }
      .distinct
      .filterNot(nativeTypeMapping.contains)
      .filterNot(importName => StringUtils.excludeLastPart(importName) == namespace.fullName)
      .map(ScalaAST.Import)
      .force
  }

  private def extractFromVerification(verification: Verification): Seq[String] = {
    extractFromDefinedFunction(verification.function)
  }

  private def extractFromDefinedFunction(definedFunction: DefinedFunction): Seq[String] = {
    val parameterTypes = definedFunction.parameters.flatMap(extractFromParameter)
    val bodyTypes = extractFromExpression(definedFunction.body)
    (parameterTypes ++ bodyTypes)
      .filterNot(definedFunction.genericTypes.contains)
  }

  private def extractFromParameter(parameter: ParameterDefinition): Seq[String] = {
    extractFromTypeReference(parameter.typeReference)
  }

  private def extractFromTypeReference(typeReference: AbstractTypeReference): Seq[String] = {
    typeReference match {
      case TypeReference(typeName, genericTypes) => typeName +: genericTypes.flatMap(extractFromTypeReference)
      case LambdaReference(inputTypes, _) => inputTypes.flatMap(extractFromTypeReference)
      case NamedFunctionReference(functionName) => Seq(functionName)
    }
  }

  private def extractFromNamedFunction(namedFunction: NamedFunction): Seq[String] = {
    val parameterTypes = namedFunction.parameters.flatMap(extractFromParameter)
    val bodyTypes = extractFromExpression(namedFunction.body)
    val returnType = extractFromTypeReference(namedFunction.returnType)
    (parameterTypes ++ bodyTypes ++ returnType)
      .filterNot(namedFunction.genericTypes.contains)
  }

  private def extractFromClassDefinition(classDefinition: ClassDefinition): Seq[String] = {
    classDefinition match {
      case aliasType: AliasType => extractFromAliasType(aliasType)
      case definedType: DefinedType => extractFromDefinedType(definedType)
      case _ => Seq.empty
    }
  }

  private def extractFromAliasType(aliasType: AliasType): Seq[String] = {
    val aliasTypes = extractFromTypeReference(aliasType.alias)
    val verificationTypes = aliasType.inherited.flatMap(extractFromVerificationReference)
    (aliasTypes ++ verificationTypes)
      .filterNot(aliasType.genericTypes.contains)
  }

  private def extractFromVerificationReference(verificationReference: VerificationReference): Seq[String] = {
    Seq(verificationReference.verificationName)
  }

  private def extractFromDefinedType(definedType: DefinedType): Seq[String] = {
    val verificationTypes = definedType.inherited.flatMap(extractFromVerificationReference)
    val attributeTypes = definedType.attributes.flatMap(extractFromAttribute)
    val expressionTypes = definedType.verifications.flatMap(verification => extractFromDefinedFunction(verification.function))
    (verificationTypes ++ attributeTypes ++ expressionTypes)
      .filterNot(definedType.genericTypes.contains)
  }

  private def extractFromAttribute(attributeDefinition: AttributeDefinition): Seq[String] = {
    val attributeType = extractFromTypeReference(attributeDefinition.typeReference)
    val verificationTypes = attributeDefinition.verifications.flatMap(extractFromVerificationReference)
    attributeType ++ verificationTypes
  }

  private def extractFromExpression(expression: Expression): Seq[String] = {
    expression match {
      case logical: LogicalExpression => extractFromExpression(logical.left) ++ extractFromExpression(logical.right)
      case calculator: CalculatorExpression => extractFromExpression(calculator.left) ++ extractFromExpression(calculator.right)
      case not: Not => extractFromExpression(not.inner)
      case reference: Reference =>
        reference.returnType match {
          case NamedFunctionReference(functionName) => Seq(functionName)
          case _ => Seq.empty
        }
      case methodCall: MethodCall => extractFromExpression(methodCall.expression) ++ methodCall.parameters.flatMap(extractFromExpression)
      case attributeCall: AttributeCall => extractFromExpression(attributeCall.expression)
      case combinedExpression: CombinedExpression => combinedExpression.parts.flatMap(extractFromExpression)
      case condition: Condition => extractFromExpression(condition.condition) ++ extractFromExpression(condition.onTrue) ++ condition.onFalse.toSeq.flatMap(extractFromExpression)
      case lambdaExpression: LambdaExpression => lambdaExpression.parameterList.flatMap(extractFromParameter) ++ extractFromExpression(lambdaExpression.expression)
      case functionCall: FunctionCall => functionCall.name +: functionCall.parameters.flatMap(extractFromExpression)
      case _ => Seq.empty
    }
  }
}
