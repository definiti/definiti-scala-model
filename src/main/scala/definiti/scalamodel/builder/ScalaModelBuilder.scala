package definiti.scalamodel.builder

import definiti.core.ast.{Library, Namespace, Root}
import definiti.scalamodel.builder.typeVerification.TypeVerificationBuilder
import definiti.scalamodel.{Configuration, ScalaAST}

class ScalaModelBuilder(val config: Configuration, val library: Library)
  extends CommonBuilder
    with ClassDefinitionBuilder
    with ExpressionBuilder
    with ImportExtractor
    with JsonBuilder
    with NamedFunctionBuilder
    with PackageBuilder
    with TypeBuilder
    with TypeVerificationBuilder
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
      imports = extractImportsFromNamespace(namespace) ++ jsonImports,
      elements = directElements ++ packageElements
    )
  }

  implicit def valueToValueSeq[A](value: A): Seq[A] = Seq(value)

  implicit def valueToValueOption[A](value: A): Option[A] = Some(value)

  implicit def optionToSeq[A](option: Option[A]): Seq[A] = option match {
    case Some(value) => Seq(value)
    case None => Nil
  }

  implicit def typeToString(typ: ScalaAST.Type): String = typ.toCode

  implicit def stringToExpression(rawStatement: String): ScalaAST.Expression = ScalaAST.SimpleExpression(rawStatement)
}