package definiti.scalamodel

import java.nio.file.{Files, Path, Paths}

import com.typesafe.config.{Config, ConfigFactory}
import com.typesafe.scalalogging.Logger

private[scalamodel] class Configuration(config: Config) {
  private val logger = Logger(getClass)

  def this() {
    this(ConfigFactory.load().getConfig("definiti.scalamodel"))
  }

  lazy val destination: Path = getPathOrElse("destination", Paths.get("target", "scalamodel"))

  private def getPathOrElse(configurationPath: String, defaultValue: => Path, mustExist: Boolean = false): Path = {
    if (config.hasPath(configurationPath)) {
      val rawPath = config.getString(configurationPath)
      val path = Paths.get(rawPath)
      if (!mustExist || Files.exists(path)) {
        path
      } else {
        logger.warn(s"Path $rawPath not found, using default one.")
        defaultValue
      }
    } else {
      defaultValue
    }
  }
}