package definiti.scalamodel.builder

import definiti.core.ast._
import definiti.scalamodel.ScalaAST

trait EnumBuilder {
  self: ScalaModelBuilder =>

  def generateEnum(enum: Enum): Seq[ScalaAST.TopLevelElement] = Seq {
    ScalaAST.ObjectDef(
      name = enum.name,
      extendz = Some(ScalaAST.Extends(ScalaAST.Type("Enumeration"), Seq.empty)),
      body = Seq(ScalaAST.SimpleExpression(s"val ${enum.cases.map(_.name).mkString(", ")} = Value")),
      property = None
    )
  }
}
