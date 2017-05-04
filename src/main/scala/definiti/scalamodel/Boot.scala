package definiti.scalamodel

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths, StandardOpenOption}

import definiti.core._

import scala.collection.JavaConverters._

object Boot extends App {
  try {
    val configuration = Configuration(
      source = Paths.get("src", "main", "resources", "samples", "first.def"),
      core = CoreConfiguration(
        source = Paths.get("src", "main", "resources", "api")
      )
    )
    val destination = Paths.get("target", "samples", "result.scala")

    val project = new Project(configuration)
    project.load() match {
      case Left(errors) =>
        errors.foreach(System.err.println)
      case Right(projectResult) =>
        val root = projectResult.root
        implicit val contexte = projectResult.context
        val result = ScalaASTBuilder.build(root)
        Files.createDirectories(destination.getParent)
        Files.write(destination, Seq(result).asJava, StandardCharsets.UTF_8, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
    }
  } catch {
    // In some cases, an Exception is thrown because the parser do not recognize an expression and crash its tree.
    // Did not happened with a successful syntax yet.
    case e: Exception =>
      e.printStackTrace()
  }
}
