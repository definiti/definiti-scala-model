package definiti.scalamodel.builder

import definiti.core.ast.Namespace
import definiti.scalamodel.utils.StringUtils
import definiti.scalamodel.{ScalaAST, ScalaCodeGenerator}

import scala.collection.mutable.ListBuffer

trait PackageBuilder {
  self: ScalaModelBuilder =>

  def buildNamespace(namespace: Namespace): Option[String] = {
    val content = buildNamespaceContent(namespace)

    if (content.nonEmpty) {
      val generatedCode = ScalaAST.StatementsGroup(packageLine(namespace))
        .plus(importLines)
        .plus(ScalaAST.PackageDef(
          name = namespace.name,
          body = content
        ))
      Some(ScalaCodeGenerator(generatedCode))
    } else {
      None
    }
  }

  private def buildNamespaceContent(namespace: Namespace): Seq[ScalaAST.Statement] = {
    val bodyBuffer = ListBuffer[ScalaAST.Statement]()
    bodyBuffer.appendAll(verificationsStatement(namespace))
    bodyBuffer.appendAll(namedFunctionsStatement(namespace))
    bodyBuffer.appendAll(classDefinitionsStatement(namespace))
    bodyBuffer
  }

  private def packageLine(namespace: Namespace): ScalaAST.PackageDeclaration = {
    ScalaAST.PackageDeclaration(StringUtils.excludeLastPart(namespace.fullName, '.'))
  }

  private def verificationsStatement(namespace: Namespace): Seq[ScalaAST.Statement] = {
    verificationsFromNamespace(namespace)
      .map(generateVerification)
  }

  private def namedFunctionsStatement(namespace: Namespace): Seq[ScalaAST.Statement] = {
    namedFunctionsFromNamespace(namespace)
      .map(generateNamedFunction)
  }

  private def classDefinitionsStatement(namespace: Namespace): Seq[ScalaAST.Statement] = {
    classDefinitionsFromNamespace(namespace)
      .flatMap(generateClassDefinition)
  }
}