package definiti.scalamodel

import java.nio.file.{Path, Paths}

import definiti.core._

object Boot {
  def main(args: Array[String]): Unit = {
    try {
      val configuration = ScalaModelConfiguration(
        core = Configuration(
          source = getSource(args),
          core = CoreConfiguration(
            source = getCoreSource(args)
          )
        ),
        destination = getDestination(args)
      )

      val project = new ScalaModelProject(configuration)
      project.loadAndWrite() match {
        case Left(errors) =>
          errors.foreach(System.err.println)
        case Right(_) =>
          println("done")
      }
    } catch {
      // In some cases, an Exception is thrown because the parser do not recognize an expression and crash its tree.
      // Did not happened with a successful syntax yet.
      case e: Exception =>
        e.printStackTrace()
    }
  }

  def getSource(args: Array[String]): Path = {
    args.find(_.startsWith("source="))
      .map(_.drop("source=".length))
      .map(Paths.get(_))
      .getOrElse(Paths.get("src", "main", "resources", "samples", "first.def"))
  }

  def getCoreSource(args: Array[String]): Path = {
    args.find(_.startsWith("core="))
      .map(_.drop("core=".length))
      .map(Paths.get(_))
      .getOrElse(Paths.get("src", "main", "resources", "api"))
  }

  def getDestination(args: Array[String]): Path = {
    args.find(_.startsWith("destination="))
      .map(_.drop("destination=".length))
      .map(Paths.get(_))
      .getOrElse(Paths.get("target", "samples"))
  }
}
