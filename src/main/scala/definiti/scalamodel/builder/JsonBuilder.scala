package definiti.scalamodel.builder

import definiti.core.ast.DefinedType
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

  def jsonImports: Seq[ScalaAST.Import] = {
    jsonBuilder.jsonImports
  }
}

trait JsonBuilderStrategy {
  def jsonImports: Seq[ScalaAST.Import]

  def buildWithValidation(definedType: DefinedType): Seq[ScalaAST.Statement]

  def buildWithoutValidation(definedType: DefinedType): Seq[ScalaAST.Statement]
}

object JsonBuilderStrategy {
  def extractUsedTypes(definedType: DefinedType, builder: ScalaModelBuilder): Seq[String] = {
    definedType
      .attributes
      .map(_.typeDeclaration)
      .map(builder.generateScalaType)
      .flatMap(extractConcreteTypes(_, builder))
      .filterNot(builder.isNative)
      .distinct
  }

  private def extractConcreteTypes(typ: ScalaAST.Type, builder: ScalaModelBuilder): Seq[String] = {
    val mainConcreteType = typ.name
    val genericConcreteTypes = typ.generics.flatMap(extractConcreteTypes(_, builder))
    mainConcreteType +: genericConcreteTypes
  }
}

class SprayJsonBuilder(val builder: ScalaModelBuilder) extends JsonBuilderStrategy {
  override def jsonImports: Seq[ScalaAST.Import] = Seq(
    ScalaAST.Import("spray.json.RootJsonFormat"),
    ScalaAST.Import("definiti.native.JsonSpraySupport._")
  )

  def buildWithValidation(definedType: DefinedType): Seq[ScalaAST.Statement] = {
    Seq(
      ScalaAST.ClassVal(
        name = s"rawFormat",
        typ = s"RootJsonFormat[${definedType.name}]",
        body = rawFormatBody(definedType)
      ),
      ScalaAST.ClassVal(
        name = s"format",
        typ = s"RootJsonFormat[${definedType.name}]",
        isImplicit = true,
        body = Seq(ScalaAST.CallFunction(
          "formatWithValidation",
          ScalaAST.SimpleExpression("rawFormat"),
          ScalaAST.SimpleExpression("verification")
        ))
      )
    )
  }

  private def rawFormatBody(definedType: DefinedType): Seq[ScalaAST.Statement] = {
    implicitAttributes(definedType) :+ rawFormatCall(definedType)
  }

  private def implicitAttributes(definedType: DefinedType): Seq[ScalaAST.Statement] = {
    JsonBuilderStrategy.extractUsedTypes(definedType, builder)
      .map { mainType =>
        ScalaAST.Val(
          name = s"${mainType}Format",
          value = ScalaAST.CallAttribute(mainType, "rawFormat"),
          isImplicit = true
        )
      }
  }

  private def rawFormatCall(definedType: DefinedType): ScalaAST.Expression = {
    ScalaAST.CallFunction(
      s"jsonFormat${definedType.attributes.length}",
      ScalaAST.SimpleExpression(s"${definedType.name}.apply")
    )
  }

  def buildWithoutValidation(definedType: DefinedType): Seq[ScalaAST.Statement] = {
    Seq(
      ScalaAST.ClassVal(
        name = s"rawFormat",
        typ = s"RootJsonFormat[${definedType.name}]",
        isImplicit = true,
        body = rawFormatBody(definedType)
      )
    )
  }
}

class PlayJsonBuilder(val builder: ScalaModelBuilder) extends JsonBuilderStrategy {
  override def jsonImports: Seq[ScalaAST.Import] = Seq(
    ScalaAST.Import("play.api.libs.json._"),
    ScalaAST.Import("definiti.native.JsonPlaySupport._")
  )

  def buildWithValidation(definedType: DefinedType): Seq[ScalaAST.Statement] = {
    Seq(
      ScalaAST.ClassVal(
        name = s"rawFormat",
        typ = s"OFormat[${definedType.name}]",
        body = rawFormatBody(definedType)
      ),
      ScalaAST.ClassVal(
        name = s"format",
        typ = s"OFormat[${definedType.name}]",
        isImplicit = true,
        body = Seq(ScalaAST.CallFunction(
          "formatWithValidation",
          ScalaAST.SimpleExpression("rawFormat"),
          ScalaAST.SimpleExpression("verification")
        ))
      )
    )
  }

  private def rawFormatBody(definedType: DefinedType): Seq[ScalaAST.Statement] = {
    implicitAttributes(definedType) :+ rawFormatCall(definedType)
  }

  private def implicitAttributes(definedType: DefinedType): Seq[ScalaAST.Statement] = {
    JsonBuilderStrategy.extractUsedTypes(definedType, builder)
      .map { mainType =>
        ScalaAST.Val(
          name = s"${mainType}Format",
          value = ScalaAST.CallAttribute(mainType, "rawFormat"),
          isImplicit = true
        )
      }
  }

  private def rawFormatCall(definedType: DefinedType): ScalaAST.Expression = {
    ScalaAST.CallAttribute("Json", s"format[${definedType.name}]")
  }

  def buildWithoutValidation(definedType: DefinedType): Seq[ScalaAST.Statement] = {
    Seq(
      ScalaAST.ClassVal(
        name = s"rawFormat",
        typ = s"OFormat[${definedType.name}]",
        isImplicit = true,
        body = rawFormatBody(definedType)
      )
    )
  }
}

object NoJsonBuilder extends JsonBuilderStrategy {
  override def jsonImports: Seq[ScalaAST.Import] = Seq.empty

  override def buildWithValidation(definedType: DefinedType): Seq[ScalaAST.Statement] = Seq.empty

  override def buildWithoutValidation(definedType: DefinedType): Seq[ScalaAST.Statement] = Seq.empty
}