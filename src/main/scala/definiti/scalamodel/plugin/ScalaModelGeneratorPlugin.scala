package definiti.scalamodel.plugin

import java.nio.file.Path

import definiti.core._
import definiti.core.ast.{Library, Root}
import definiti.scalamodel.Configuration
import definiti.scalamodel.builder.ScalaModelBuilder
import definiti.scalamodel.generator.ScalaProjectGenerator
import definiti.scalamodel.utils.Resource

class ScalaModelGeneratorPlugin extends GeneratorPlugin {
  val config = new Configuration()

  override def name: String = "scala-model-generator"

  override def generate(root: Root, library: Library): Map[Path, String] = {
    nativeSources ++ generatedSources(root, library)
  }

  def nativeSources: Map[Path, String] = {
    val source = Resource("native")
    val destinationDirectory = config.destination.resolve("definiti").resolve("native")
    source.children
      .map(file => destinationDirectory.resolve(file.name) -> file.content)
      .toMap
  }

  def generatedSources(root: Root, library: Library): Map[Path, String] = {
    val scalaRoot = new ScalaModelBuilder(config, library).build(root)
    val scalaFiles = new ScalaProjectGenerator(config).generateProject(scalaRoot)
    scalaFiles
      .map(scalaFile => scalaFile.path -> scalaFile.content)
      .toMap
  }
}