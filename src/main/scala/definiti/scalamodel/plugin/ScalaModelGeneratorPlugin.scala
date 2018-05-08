package definiti.scalamodel.plugin

import java.nio.file.Path

import definiti.common.ast.{Library, Root}
import definiti.common.plugin.GeneratorPlugin
import definiti.scalamodel.builder.ScalaModelBuilder
import definiti.scalamodel.generator.ScalaProjectGenerator
import definiti.scalamodel.utils.Resource
import definiti.scalamodel.{FileConfiguration, JsonFormat}

class ScalaModelGeneratorPlugin extends GeneratorPlugin {
  val config = new FileConfiguration()
  private val nativeSourceDirectory: Resource = Resource("native")
  private val destinationDirectory: Path = config.destination.resolve("definiti").resolve("native")

  override def name: String = "scala-model-generator"

  override def generate(root: Root, library: Library): Map[Path, String] = {
    nativeSources ++ generatedSources(root, library)
  }

  def nativeSources: Map[Path, String] = {
    val source = nativeSourceDirectory
    val commonSources = source.children
      .filterNot(_.isDirectory)
      .map(file => destinationDirectory.resolve(file.name) -> file.content)
      .toMap
    commonSources ++ jsonSources
  }

  private def jsonSources: Map[Path, String] = {
    config.json.format match {
      case JsonFormat.play =>
        Map(destinationDirectory.resolve("JsonPlaySupport.scala") -> nativeSourceDirectory.resolve("json").resolve("JsonPlaySupport.scala").content)
      case JsonFormat.spray =>
        Map(destinationDirectory.resolve("JsonSpraySupport.scala") -> nativeSourceDirectory.resolve("json").resolve("JsonSpraySupport.scala").content)
      case JsonFormat.none =>
        Map.empty
    }
  }

  def generatedSources(root: Root, library: Library): Map[Path, String] = {
    val scalaRoot = new ScalaModelBuilder(config, library).build(root)
    val scalaFiles = new ScalaProjectGenerator(config).generateProject(scalaRoot)
    scalaFiles
      .map(scalaFile => scalaFile.path -> scalaFile.content)
      .toMap
  }
}