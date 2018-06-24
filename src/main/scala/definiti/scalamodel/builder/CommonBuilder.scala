package definiti.scalamodel.builder

import definiti.common.ast._
import definiti.scalamodel.ScalaAST

trait CommonBuilder {
  self: ScalaModelBuilder =>

  val nativeTypeMapping = Map(
    "Any" -> "Any",
    "Boolean" -> "Boolean",
    "Date" -> "LocalDateTime",
    "List" -> "Seq",
    "Number" -> "BigDecimal",
    "Option" -> "Option",
    "String" -> "String",
    "Unit" -> "Unit",
    "OkKo" -> "Option[Message]"
  )

  def verificationsFromNamespace(namespace: Namespace): Seq[Verification] = {
    namespace.elements.collect {
      case verification: Verification => verification
    }
  }

  def namedFunctionsFromNamespace(namespace: Namespace): Seq[NamedFunction] = {
    namespace.elements.collect {
      case namedFunction: NamedFunction => namedFunction
    }
  }

  def classDefinitionsFromNamespace(namespace: Namespace): Seq[ClassDefinition] = {
    namespace.elements.collect {
      case classDefinition: ClassDefinition => classDefinition
    }
  }

  def definedTypesFromNamespace(namespace: Namespace): Seq[DefinedType] = {
    namespace.elements.collect {
      case definedType: DefinedType => definedType
    }
  }

  def aliasTypeFromNamespace(namespace: Namespace): Seq[AliasType] = {
    namespace.elements.collect {
      case aliasType: AliasType => aliasType
    }
  }

  def namespacesFromNamespace(namespace: Namespace): Seq[Namespace] = {
    namespace.elements.collect {
      case namespace: Namespace => namespace
    }
  }

  def generateDef(name: String, definedFunction: DefinedFunction, property: Option[String] = None): ScalaAST.Def1 = {
    ScalaAST.Def1(
      name = name,
      typ = generateType(definedFunction.body.returnType),
      generics = definedFunction.genericTypes,
      parameters = definedFunction.parameters.map(generateParameter),
      body = generateExpression(definedFunction.body),
      property = property
    )
  }
}
