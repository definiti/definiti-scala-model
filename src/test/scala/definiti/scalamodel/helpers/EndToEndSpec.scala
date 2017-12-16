package definiti.scalamodel.helpers

import java.nio.file.Paths

import definiti.core.{Configuration => CoreConfiguration, _}
import definiti.scalamodel.builder.ScalaModelBuilder
import definiti.scalamodel.{Configuration, ScalaAST}
import org.scalatest.{FlatSpec, Matchers}

trait EndToEndSpec extends FlatSpec with Matchers with ASTMatcher {
  def processDirectory(sample: String, configuration: Configuration = ConfigurationMock()): Validated[ScalaAST.Root] = {
    process(configurationDirectory(sample), configuration)
  }

  def configurationDirectory(sample: String): CoreConfiguration = {
    CoreConfigurationMock(
      source = Paths.get(s"src/test/resources/samples/${sample.replaceAll("\\.", "/")}"),
      apiSource = Paths.get(s"src/main/resources/api"),
      contexts = Seq()
    )
  }

  def processFile(sample: String, configuration: Configuration = ConfigurationMock()): Validated[ScalaAST.Root] = {
    process(configurationFile(sample), configuration)
  }

  def configurationFile(sample: String): CoreConfiguration = {
    CoreConfigurationMock(
      source = Paths.get(s"src/test/resources/samples/${sample.replaceAll("\\.", "/")}.def"),
      apiSource = Paths.get(s"src/main/resources/api"),
      contexts = Seq()
    )
  }

  private def process(coreConfiguration: CoreConfiguration, configuration: Configuration): Validated[ScalaAST.Root] = {
    val project = new Project(coreConfiguration)
    project.generateStructureWithLibrary()
      .map { case (ast, library) =>
        val builder = new ScalaModelBuilder(configuration, library)
        builder.build(ast)
      }
  }
}