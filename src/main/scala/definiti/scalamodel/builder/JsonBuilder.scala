package definiti.scalamodel.builder

import definiti.core.ast.DefinedType
import definiti.scalamodel.{Configuration, JsonFormat, JsonValidation, ScalaAST}

trait JsonBuilder {
  self: ScalaModelBuilder =>

  private val sprayJsonBuilder: JsonBuilderStrategy = new SprayJsonBuilder(config)

  def buildJsonConverter(definedType: DefinedType): Seq[ScalaAST.Statement] = {
    config.json.format match {
      case JsonFormat.spray => sprayJsonBuilder.build(definedType)
      case JsonFormat.none => Seq.empty
    }
  }
}

trait JsonBuilderStrategy {
  def build(definedType: DefinedType): Seq[ScalaAST.Statement]
}

class SprayJsonBuilder(config: Configuration) extends JsonBuilderStrategy {
  override def build(definedType: DefinedType): Seq[ScalaAST.Statement] = {
    config.json.validation match {
      case JsonValidation.flat => buildWithFlatValidation(definedType)
      case JsonValidation.none => buildWithoutValidation(definedType)
    }
  }

  private def buildWithFlatValidation(definedType: DefinedType): Seq[ScalaAST.Statement] = {
    Seq(
      ScalaAST.Import("spray.json.RootJsonFormat"),
      ScalaAST.Import("definiti.native.JsonSpraySupport._"),
      ScalaAST.ClassVal(
        name = s"${definedType.name}Format",
        typ = s"RootJsonFormat[${definedType.name}]",
        isImplicit = true,
        body = Seq(
          ScalaAST.CallFunction(
            target = ScalaAST.SimpleExpression("formatWithValidation"),
            arguments = Seq(
              ScalaAST.CallFunction(
                s"jsonFormat${definedType.attributes.length}",
                ScalaAST.SimpleExpression(s"${definedType.name}.apply")
              ),
              ScalaAST.SimpleExpression("allVerifications")
            )
          )
        )
      )
    )
  }

  private def buildWithoutValidation(definedType: DefinedType): Seq[ScalaAST.Statement] = {
    Seq(
      ScalaAST.Import("spray.json.RootJsonFormat"),
      ScalaAST.Import("definiti.native.JsonSpraySupport._"),
      ScalaAST.ClassVal(
        name = s"${definedType.name}Format",
        typ = s"RootJsonFormat[${definedType.name}]",
        isImplicit = true,
        body = Seq(ScalaAST.CallFunction(
          s"jsonFormat${definedType.attributes.length}",
          ScalaAST.SimpleExpression(s"${definedType.name}.apply")
        ))
      )
    )
  }
}