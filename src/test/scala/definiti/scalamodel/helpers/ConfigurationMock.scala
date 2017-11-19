package definiti.scalamodel.helpers

import java.nio.file.{Path, Paths}

import definiti.core._

case class ConfigurationMock(
  source: Path = Paths.get(""),
  apiSource: Path = Paths.get(""),
  parsers: Seq[ParserPlugin] = Seq.empty,
  validators: Seq[ValidatorPlugin] = Seq.empty,
  generators: Seq[GeneratorPlugin] = Seq.empty,
  contexts: Seq[ContextPlugin[_]] = Seq.empty
) extends Configuration