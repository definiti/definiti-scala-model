package definiti.scalamodel.builder

import definiti.core.ast.{Library, Namespace, Root}
import definiti.scalamodel.utils.StringUtils
import definiti.scalamodel.{Configuration, ScalaAST}

class ScalaModelBuilder(config: Configuration, val library: Library)
  extends CommonBuilder
    with ClassDefinitionBuilder
    with ExpressionBuilder
    with NamedFunctionBuilder
    with PackageBuilder
    with TypeBuilder
    with VerificationBuilder {

  def buildScalaFiles(root: Root): Seq[ScalaAST.ScalaFile] = {
    val rootNamespace = Namespace(
      name = "",
      fullName = "",
      elements = root.elements
    )
    buildNamespaceFiles(rootNamespace)
  }

  private def buildNamespaceFiles(namespace: Namespace): Seq[ScalaAST.ScalaFile] = {
    val packageFile = buildNamespace(namespace).map(buildScalaFile(namespace.fullName, _))
    val subPackageFiles = namespace.elements.collect {
      case subNamespace: Namespace => buildNamespaceFiles(subNamespace)
    }.flatten
    (packageFile ++ subPackageFiles).toSeq
  }

  private def buildScalaFile(packageName: String, content: String): ScalaAST.ScalaFile = {
    val dirname = StringUtils.excludeLastPart(packageName, '.').replaceAllLiterally(".", "/")
    val filename = StringUtils.lastPart(packageName, '.')
    val path = config.destination.resolve(dirname).resolve(s"$filename.scala")
    ScalaAST.ScalaFile(path, content)
  }
}