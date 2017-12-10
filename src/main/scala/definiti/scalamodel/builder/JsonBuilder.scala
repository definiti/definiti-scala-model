package definiti.scalamodel.builder

import definiti.core.ast.DefinedType
import definiti.scalamodel.{Configuration, JsonFormat, JsonValidation, ScalaAST}

trait JsonBuilder {
  self: ScalaModelBuilder =>

  private val jsonBuilder: JsonBuilderStrategy = config.json.format match {
    case JsonFormat.spray => SprayJsonBuilder
    case JsonFormat.play => PlayJsonBuilder
    case JsonFormat.none => NoJsonBuilder
  }

  def buildJsonConverter(definedType: DefinedType): Seq[ScalaAST.Statement] = {
    config.json.validation match {
      case JsonValidation.flat => jsonBuilder.buildWithFlatValidation(definedType)
      case JsonValidation.none => jsonBuilder.buildWithoutValidation(definedType)
    }
  }
}

trait JsonBuilderStrategy {
  def buildWithFlatValidation(definedType: DefinedType): Seq[ScalaAST.Statement]

  def buildWithoutValidation(definedType: DefinedType): Seq[ScalaAST.Statement]
}

object SprayJsonBuilder extends JsonBuilderStrategy {
  def buildWithFlatValidation(definedType: DefinedType): Seq[ScalaAST.Statement] = {
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

  def buildWithoutValidation(definedType: DefinedType): Seq[ScalaAST.Statement] = {
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

object PlayJsonBuilder extends JsonBuilderStrategy {
  def buildWithFlatValidation(definedType: DefinedType): Seq[ScalaAST.Statement] = {
    Seq(
      ScalaAST.Import("play.api.libs.json._"),
      ScalaAST.Import("definiti.native.JsonPlaySupport._"),
      ScalaAST.ClassVal(
        name = s"${definedType.name}Format",
        typ = s"OFormat[${definedType.name}]",
        isImplicit = true,
        body = Seq(ScalaAST.CallFunction(
          "formatWithValidation",
          ScalaAST.CallAttribute("Json", s"format[${definedType.name}]"),
          ScalaAST.SimpleExpression("allVerifications")
        ))
      )
    )
  }

  def buildWithoutValidation(definedType: DefinedType): Seq[ScalaAST.Statement] = {
    Seq(
      ScalaAST.Import("play.api.libs.json._"),
      ScalaAST.Import("definiti.native.JsonPlaySupport._"),
      ScalaAST.ClassVal(
        name = s"${definedType.name}Format",
        typ = s"OFormat[${definedType.name}]",
        isImplicit = true,
        body = Seq(ScalaAST.CallAttribute("Json", s"format[${definedType.name}]"))
      )
    )
  }
}

object NoJsonBuilder extends JsonBuilderStrategy {
  override def buildWithFlatValidation(definedType: DefinedType): Seq[ScalaAST.Statement] = Seq.empty

  override def buildWithoutValidation(definedType: DefinedType): Seq[ScalaAST.Statement] = Seq.empty
}