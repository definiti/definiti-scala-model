package definiti.scalamodel.builder

import definiti.core.ast.{Library, Namespace, Root}
import definiti.scalamodel.{Configuration, ScalaAST}

class ScalaModelBuilder(val config: Configuration, val library: Library)
  extends CommonBuilder
    with ClassDefinitionBuilder
    with ExpressionBuilder
    with JsonBuilder
    with NamedFunctionBuilder
    with PackageBuilder
    with TypeBuilder
    with VerificationBuilder {

  def build(root: Root): ScalaAST.Root = {
    val rootNamespace = Namespace(
      name = "",
      fullName = "",
      elements = root.elements
    )
    val directElements = buildNamespaceContent(rootNamespace)
    val packageElements = root.elements.collect {
      case namespace: Namespace => buildPackage(namespace)
    }
    ScalaAST.Root(
      elements = directElements ++ packageElements
    )
  }

  private def buildPackage(namespace: Namespace): ScalaAST.Package = {
    val directElements = buildNamespaceContent(namespace)
    val packageElements = namespace.elements.collect {
      case namespace: Namespace => buildPackage(namespace)
    }
    ScalaAST.Package(
      namespace.fullName,
      elements = directElements ++ packageElements
    )
  }
}