package definiti.scalamodel.json

import definiti.scalamodel.JsonFormat
import definiti.scalamodel.ScalaAST._
import definiti.scalamodel.helpers.EndToEndSpec

class SprayJsonSpec extends EndToEndSpec {
  import SprayJsonSpec._

  "The generator" should "generate the simple json of all defined type when no validation" in {
    val expected = definedTypeSample(false)
    val output = processFile("json.definedType", configuration(false))
    output should beValidRoot(expected)
  }

  it should "generate the json of all defined type with flat validation" in {
    val expected = definedTypeSample(true)
    val output = processFile("json.definedType", configuration(true))
    output should beValidRoot(expected)
  }
}

object SprayJsonSpec extends BaseJsonSpec {
  override protected def jsonFormat = JsonFormat.spray

  private val imports = Seq(Import("spray.json.RootJsonFormat"), Import("definiti.native.JsonSpraySupport._"))

  protected def firstDefinedTypeJsonWithValidation = {
    imports ++ Seq(
      rawFormat("MyFirstType", 1),
      implicitFormat("MyFirstType")
    )
  }

  protected def firstDefinedTypeJsonWithoutValidation = {
    imports :+
      ClassVal(
        name = s"MyFirstTypeFormat",
        typ = s"RootJsonFormat[MyFirstType]",
        body = Seq(CallFunction(s"jsonFormat1", SimpleExpression(s"MyFirstType.apply"))),
        isImplicit = true
      )
  }

  protected def secondDefinedTypeJsonWithValidation = {
    imports ++ Seq(
      rawFormat("MySecondType", 6, "MyFirstType", "MyThirdType"),
      implicitFormat("MySecondType")
    )
  }

  protected def secondDefinedTypeJsonWithoutValidation = {
    imports :+
      ClassVal(
        name = s"MySecondTypeFormat",
        typ = s"RootJsonFormat[MySecondType]",
        body = Seq(CallFunction(s"jsonFormat6", SimpleExpression(s"MySecondType.apply"))),
        isImplicit = true
      )
  }

  protected def thirdDefinedTypeJsonWithValidation = {
    imports ++ Seq(
      rawFormat("MyThirdType", 1),
      implicitFormat("MyThirdType")
    )
  }

  protected def thirdDefinedTypeJsonWithoutValidation = {
    imports :+
      ClassVal(
        name = s"MyThirdTypeFormat",
        typ = s"RootJsonFormat[MyThirdType]",
        body = Seq(CallFunction(s"jsonFormat1", SimpleExpression(s"MyThirdType.apply"))),
        isImplicit = true
      )
  }

  private def rawFormat(typeName: String, attributesCount: Int, importedRawFormats: String*) = {
    def importedRawFormatStatement(name: String): Statement = {
      Val(
        name = s"${name}Format",
        value = CallAttribute(name, s"${name}RawFormat"),
        isImplicit = true
      )
    }

    ClassVal(
      name = s"${typeName}RawFormat",
      typ = s"RootJsonFormat[${typeName}]",
      body =
        importedRawFormats.map(importedRawFormatStatement) :+
          CallFunction(
            target = s"jsonFormat${attributesCount}",
            SimpleExpression(s"${typeName}.apply")
          )
    )
  }

  private def implicitFormat(typeName: String) = {
    ClassVal(
      name = s"${typeName}Format",
      typ = s"RootJsonFormat[${typeName}]",
      body = Seq(
        CallFunction(
          target = "formatWithValidation",
          SimpleExpression(s"${typeName}RawFormat"),
          SimpleExpression("allVerifications")
        )
      ),
      isImplicit = true
    )
  }
}