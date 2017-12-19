package definiti.scalamodel.builder

import definiti.core.ast.{DefinedType, TypeReference}
import definiti.scalamodel.{JsonFormat, ScalaAST}

trait JsonBuilder {
  self: ScalaModelBuilder =>

  private val jsonBuilder: JsonBuilderStrategy = config.json.format match {
    case JsonFormat.spray => new SprayJsonBuilder(this)
    case JsonFormat.play => new PlayJsonBuilder(this)
    case JsonFormat.none => NoJsonBuilder
  }

  def buildJsonConverter(definedType: DefinedType): Seq[ScalaAST.Statement] = {
    if (config.json.validation) {
      jsonBuilder.buildWithValidation(definedType)
    } else {
      jsonBuilder.buildWithoutValidation(definedType)
    }
  }
}

trait JsonBuilderStrategy {
  def buildWithValidation(definedType: DefinedType): Seq[ScalaAST.Statement]

  def buildWithoutValidation(definedType: DefinedType): Seq[ScalaAST.Statement]
}

object JsonBuilderStrategy {
  def extractUsedTypes(definedType: DefinedType, builder: ScalaModelBuilder): Seq[String] = {
    definedType
      .attributes
      .map(_.typeReference)
      .map(extractConcreteType)
      .filterNot(builder.isNative)
      .map(builder.generateMainType)
      .distinct
  }

  private def extractConcreteType(typeReference: TypeReference): TypeReference = {
    if (typeReference.typeName == "List" || typeReference.typeName == "Option") {
      typeReference
        .genericTypes
        .headOption
        .map(extractConcreteType)
        .getOrElse(typeReference)
    } else {
      typeReference
    }
  }
}

class SprayJsonBuilder(val builder: ScalaModelBuilder) extends JsonBuilderStrategy {
  def buildWithValidation(definedType: DefinedType): Seq[ScalaAST.Statement] = {
    Seq(
      ScalaAST.Import("spray.json.RootJsonFormat"),
      ScalaAST.Import("definiti.native.JsonSpraySupport._"),
      rawFormatWithValidation(definedType),
      ScalaAST.ClassVal(
        name = s"${definedType.name}Format",
        typ = s"RootJsonFormat[${definedType.name}]",
        isImplicit = true,
        body = Seq(formatWithValidation(definedType))
      )
    )
  }

  private def rawFormatWithValidation(definedType: DefinedType): ScalaAST.Statement = {
    ScalaAST.ClassVal(
      name = s"${definedType.name}RawFormat",
      typ = s"RootJsonFormat[${definedType.name}]",
      body = implicitAttributes(definedType) :+ rawFormatBody(definedType)
    )
  }

  private def rawFormatBody(definedType: DefinedType): ScalaAST.Expression = {
    ScalaAST.CallFunction(
      s"jsonFormat${definedType.attributes.length}",
      ScalaAST.SimpleExpression(s"${definedType.name}.apply")
    )
  }

  private def implicitAttributes(definedType: DefinedType): Seq[ScalaAST.Statement] = {
    JsonBuilderStrategy.extractUsedTypes(definedType, builder)
      .map { mainType =>
        ScalaAST.Val(
          name = s"${mainType}Format",
          value = ScalaAST.CallAttribute(mainType, s"${mainType}RawFormat"),
          isImplicit = true
        )
      }
  }

  private def formatWithValidation(definedType: DefinedType): ScalaAST.CallFunction = {
    ScalaAST.CallFunction(
      target = ScalaAST.SimpleExpression("formatWithValidation"),
      arguments = Seq(
        ScalaAST.SimpleExpression(s"${definedType.name}RawFormat"),
        ScalaAST.SimpleExpression("allVerifications")
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
        body = Seq(rawFormatBody(definedType))
      )
    )
  }
}

class PlayJsonBuilder(val builder: ScalaModelBuilder) extends JsonBuilderStrategy {
  def buildWithValidation(definedType: DefinedType): Seq[ScalaAST.Statement] = {
    Seq(
      ScalaAST.Import("play.api.libs.json._"),
      ScalaAST.Import("definiti.native.JsonPlaySupport._"),
      rawFormatWithValidation(definedType),
      ScalaAST.ClassVal(
        name = s"${definedType.name}Format",
        typ = s"OFormat[${definedType.name}]",
        isImplicit = true,
        body = Seq(ScalaAST.CallFunction(
          "formatWithValidation",
          ScalaAST.SimpleExpression(s"${definedType.name}RawFormat"),
          ScalaAST.SimpleExpression("allVerifications")
        ))
      )
    )
  }

  private def rawFormatWithValidation(definedType: DefinedType): ScalaAST.Statement = {
    ScalaAST.ClassVal(
      name = s"${definedType.name}RawFormat",
      typ = s"OFormat[${definedType.name}]",
      body = implicitAttributes(definedType) :+ rawFormatBody(definedType)
    )
  }

  private def implicitAttributes(definedType: DefinedType): Seq[ScalaAST.Statement] = {
    JsonBuilderStrategy.extractUsedTypes(definedType, builder)
      .map { mainType =>
        ScalaAST.Val(
          name = s"${mainType}Format",
          value = ScalaAST.CallAttribute(mainType, s"${mainType}RawFormat"),
          isImplicit = true
        )
      }
  }

  private def rawFormatBody(definedType: DefinedType): ScalaAST.Expression = {
    ScalaAST.CallAttribute("Json", s"format[${definedType.name}]")
  }

  def buildWithoutValidation(definedType: DefinedType): Seq[ScalaAST.Statement] = {
    Seq(
      ScalaAST.Import("play.api.libs.json._"),
      ScalaAST.Import("definiti.native.JsonPlaySupport._"),
      ScalaAST.ClassVal(
        name = s"${definedType.name}Format",
        typ = s"OFormat[${definedType.name}]",
        isImplicit = true,
        body = Seq(rawFormatBody(definedType))
      )
    )
  }
}

object NoJsonBuilder extends JsonBuilderStrategy {
  override def buildWithValidation(definedType: DefinedType): Seq[ScalaAST.Statement] = Seq.empty

  override def buildWithoutValidation(definedType: DefinedType): Seq[ScalaAST.Statement] = Seq.empty
}