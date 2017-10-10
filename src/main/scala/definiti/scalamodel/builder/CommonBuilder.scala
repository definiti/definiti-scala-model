package definiti.scalamodel.builder

import definiti.core.ast._
import definiti.scalamodel.ScalaAST

trait CommonBuilder {
  self: ScalaModelBuilder =>

  val nativeTypeMapping = Map(
    "Number" -> "BigDecimal"
  )

  val importLines: ScalaAST.StatementsGroup = {
    ScalaAST.StatementsGroup(
      ScalaAST.Import("definiti.native._"),
      ScalaAST.Import("java.util.Date")
    )
  }

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
}
