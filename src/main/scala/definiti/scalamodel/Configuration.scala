package definiti.scalamodel

import java.nio.file.{Path, Paths}

import com.typesafe.config.{Config, ConfigFactory}

private[scalamodel] trait Configuration {
  def destination: Path

  def json: JsonConfiguration
}

private[scalamodel] case class JsonConfiguration(format: JsonFormat.Value, validation: Boolean)

private[scalamodel] object JsonFormat extends Enumeration {
  val play, spray, none = Value

  def fromString(value: String): Option[JsonFormat.Value] = {
    values.find(_.toString == value)
  }
}

private[scalamodel] class FileConfiguration(config: Config) extends Configuration {
  def this() {
    this(ConfigFactory.load())
  }

  lazy val destination: Path = getFirstDefinedPath(
    "definiti.scalamodel.destination",
    "definiti.build.destination"
  ).getOrElse(Paths.get("target", "scalamodel"))

  lazy val json: JsonConfiguration = JsonConfiguration(
    format = getStringOpt("definiti.scalamodel.json.format").flatMap(JsonFormat.fromString).getOrElse(JsonFormat.none),
    validation = getBooleanOpt("definiti.scalamodel.json.validation").getOrElse(false)
  )

  private def getFirstDefinedPath(keys: String*): Option[Path] = {
    keys
      .map(getPathOpt)
      .collectFirst {
        case Some(path) => path
      }
  }

  private def getPathOpt(configurationPath: String): Option[Path] = {
    if (config.hasPath(configurationPath)) {
      val rawPath = config.getString(configurationPath)
      val path = Paths.get(rawPath)
      Some(path)
    } else {
      None
    }
  }

  private def getStringOpt(configurationPath: String): Option[String] = {
    if (config.hasPath(configurationPath)) {
      Some(config.getString(configurationPath))
    } else {
      None
    }
  }

  private def getBooleanOpt(configurationPath: String): Option[Boolean] = {
    if (config.hasPath(configurationPath)) {
      Some(config.getBoolean(configurationPath))
    } else {
      None
    }
  }
}