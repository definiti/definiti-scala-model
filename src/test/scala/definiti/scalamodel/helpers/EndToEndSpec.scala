package definiti.scalamodel.helpers

import java.nio.file.Paths

import definiti.core.{Configuration => CoreConfiguration, _}
import definiti.scalamodel.builder.ScalaModelBuilder
import definiti.scalamodel.{Configuration, ScalaAST}
import org.scalatest.{FlatSpec, Matchers}

trait EndToEndSpec extends FlatSpec with Matchers {
  def processDirectory(sample: String): Validated[ScalaAST.Root] = {
    process(configurationDirectory(sample))
  }

  def configurationDirectory(sample: String): CoreConfiguration = {
    ConfigurationMock(
      source = Paths.get(s"src/test/resources/samples/${sample.replaceAll("\\.", "/")}"),
      apiSource = Paths.get(s"src/main/resources/api"),
      contexts = Seq()
    )
  }

  def processFile(sample: String): Validated[ScalaAST.Root] = {
    process(configurationFile(sample))
  }

  def configurationFile(sample: String): CoreConfiguration = {
    ConfigurationMock(
      source = Paths.get(s"src/test/resources/samples/${sample.replaceAll("\\.", "/")}.def"),
      apiSource = Paths.get(s"src/main/resources/api"),
      contexts = Seq()
    )
  }

  private def process(configuration: CoreConfiguration): Validated[ScalaAST.Root] = {
    val project = new Project(configuration)
    project.generateStructureWithLibrary()
      .map { case (ast, library) =>
        val builder = new ScalaModelBuilder(new Configuration(), library)
        builder.build(ast)
      }
  }
}