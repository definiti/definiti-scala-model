package definiti.scalamodel.builder

import definiti.core.ast.Namespace
import definiti.scalamodel.ScalaAST

import scala.collection.mutable.ListBuffer

trait PackageBuilder {
  self: ScalaModelBuilder =>

  def buildNamespaceContent(namespace: Namespace): Seq[ScalaAST.TopLevelElement] = {
    val bodyBuffer = ListBuffer[ScalaAST.TopLevelElement]()
    bodyBuffer.appendAll(verificationsStatement(namespace))
    bodyBuffer.appendAll(namedFunctionsStatement(namespace))
    bodyBuffer.appendAll(classDefinitionsStatement(namespace))
    bodyBuffer
  }

  private def verificationsStatement(namespace: Namespace): Seq[ScalaAST.TopLevelElement] = {
    verificationsFromNamespace(namespace)
      .flatMap(generateVerification)
  }

  private def namedFunctionsStatement(namespace: Namespace): Seq[ScalaAST.TopLevelElement] = {
    namedFunctionsFromNamespace(namespace)
      .flatMap(generateNamedFunction)
  }

  private def classDefinitionsStatement(namespace: Namespace): Seq[ScalaAST.TopLevelElement] = {
    classDefinitionsFromNamespace(namespace)
      .flatMap(generateClassDefinition)
  }
}