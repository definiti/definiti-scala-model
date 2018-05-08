package definiti.scalamodel.builder

import definiti.common.ast.{Library, Namespace, Root}
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
    ScalaAST.Root(
      packages = root.namespaces
        .map(buildPackage)
        .filter(_.elements.nonEmpty)
    )
  }

  private def buildPackage(namespace: Namespace): ScalaAST.Package = {
    ScalaAST.Package(
      if (namespace.fullName.isEmpty) "root" else namespace.fullName,
      imports = extractImportsFromNamespace(namespace) ++ jsonImports,
      elements = buildNamespaceContent(namespace)
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