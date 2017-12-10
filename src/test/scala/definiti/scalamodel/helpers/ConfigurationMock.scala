package definiti.scalamodel.helpers

import java.nio.file.{Path, Paths}

import definiti.scalamodel.{Configuration, JsonConfiguration, JsonFormat, JsonValidation}

case class ConfigurationMock(
  destination: Path = Paths.get(""),
  json: JsonConfiguration = JsonConfiguration(
    format = JsonFormat.none,
    validation = JsonValidation.none
  )
) extends Configuration