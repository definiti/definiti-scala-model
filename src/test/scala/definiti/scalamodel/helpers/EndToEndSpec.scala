package definiti.scalamodel.helpers

import java.nio.file.{Files, Paths}

import definiti.common.program.{Ko, Ok, ProgramResult}
import definiti.common.tests.TestProjectExecution
import definiti.common.tests.{ConfigurationMock => CoreConfigurationMock}
import definiti.core.Project
import definiti.scalamodel.builder.ScalaModelBuilder
import definiti.scalamodel.generator.ScalaProjectGenerator
import definiti.scalamodel.{Configuration, ScalaAST}
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.JavaConverters._

trait EndToEndSpec extends FlatSpec with Matchers with TestProjectExecution with ASTMatcher {
  def processFile(sample: String): ProgramResult[ScalaAST.Root] = {
    process(configurationFile(sample), ConfigurationMock())
  }

  def configurationFile(sample: String): CoreConfigurationMock = {
    CoreConfigurationMock(
      source = Paths.get(s"src/test/resources/samples/${sample.replaceAll("\\.", "/")}.def"),
      apiSource = Paths.get(s"src/main/resources/api"),
      contexts = Seq()
    )
  }

  private def process(coreConfiguration: CoreConfigurationMock, configuration: Configuration): ProgramResult[ScalaAST.Root] = {
    val project = new Project(coreConfiguration)
    project.generateStructureWithLibrary()
      .map { case (ast, library) =>
        val builder = new ScalaModelBuilder(configuration, library)
        builder.build(ast)
      }
      .run(coreConfiguration.programConfiguration)
  }

  def processSample(sample: String, configuration: Configuration = ConfigurationMock()): Any = {
    val coreConfiguration = configurationFile(s"${sample}.input")
    process(coreConfiguration, configuration) match {
      case Ok(root, _) =>
        val scalaProjectGenerator = new ScalaProjectGenerator(configuration)
        val output = scalaProjectGenerator.generateProject(root).head.content
        val expected = readFile(s"src/test/resources/samples/${sample.replaceAll("\\.", "/")}/output.scala")
        normalizeContent(output) should ===(normalizeContent(expected))
      case ko: Ko[_] =>
        fail(s"Expected Ok, got: ${ko}")
    }
  }

  private def readFile(path: String): String = {
    Files.readAllLines(Paths.get(path))
      .asScala
      .mkString("\n")
  }

  private def normalizeContent(content: String): String = {
    content
      .replaceAllLiterally("\r\n", "\n")
      .trim
  }
}