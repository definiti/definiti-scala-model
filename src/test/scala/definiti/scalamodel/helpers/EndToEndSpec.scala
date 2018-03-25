package definiti.scalamodel.helpers

import java.nio.file.{Files, Paths}

import definiti.core.{Configuration => CoreConfiguration, _}
import definiti.scalamodel.builder.ScalaModelBuilder
import definiti.scalamodel.generator.ScalaProjectGenerator
import definiti.scalamodel.{Configuration, ScalaAST}
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.JavaConverters._

trait EndToEndSpec extends FlatSpec with Matchers with ASTMatcher {
  def processDirectory(sample: String, configuration: Configuration = ConfigurationMock()): ProgramResult[ScalaAST.Root] = {
    process(configurationDirectory(sample), configuration)
  }

  def configurationDirectory(sample: String): CoreConfiguration = {
    CoreConfigurationMock(
      source = Paths.get(s"src/test/resources/samples/${sample.replaceAll("\\.", "/")}"),
      apiSource = Paths.get(s"src/main/resources/api"),
      contexts = Seq()
    )
  }

  def processFile(sample: String, configuration: Configuration = ConfigurationMock()): ProgramResult[ScalaAST.Root] = {
    process(configurationFile(sample), configuration)
  }

  def configurationFile(sample: String): CoreConfiguration = {
    CoreConfigurationMock(
      source = Paths.get(s"src/test/resources/samples/${sample.replaceAll("\\.", "/")}.def"),
      apiSource = Paths.get(s"src/main/resources/api"),
      contexts = Seq()
    )
  }

  private def process(coreConfiguration: CoreConfiguration, configuration: Configuration): ProgramResult[ScalaAST.Root] = {
    val project = new Project(coreConfiguration)
    project.generateStructureWithLibrary()
      .map { case (ast, library) =>
        val builder = new ScalaModelBuilder(configuration, library)
        builder.build(ast)
      }
      .run(coreConfiguration)
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