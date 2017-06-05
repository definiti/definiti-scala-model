package definiti.scalamodel

import java.nio.file.Path

import definiti.core.Configuration

case class ScalaModelConfiguration(
  core: Configuration,
  destination: Path
)