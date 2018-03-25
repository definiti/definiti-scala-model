package definiti.scalamodel.helpers

import java.nio.file.{Path, Paths}

import definiti.core._
import definiti.core.validation.ControlLevel

case class CoreConfigurationMock(
  source: Path = Paths.get(""),
  apiSource: Path = Paths.get(""),
  parsers: Seq[ParserPlugin] = Seq.empty,
  validators: Seq[ValidatorPlugin] = Seq.empty,
  generators: Seq[GeneratorPlugin] = Seq.empty,
  contexts: Seq[ContextPlugin[_]] = Seq.empty,
  controlLevel: ControlLevel.Value = ControlLevel.info,
  fatalLevel: ControlLevel.Value = ControlLevel.error,
  userFlags: Map[String, ControlLevel.Value] = Map.empty
) extends Configuration