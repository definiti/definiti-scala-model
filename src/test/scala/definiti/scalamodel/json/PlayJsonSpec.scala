package definiti.scalamodel.json

import definiti.scalamodel.JsonFormat
import definiti.scalamodel.ScalaAST._
import definiti.scalamodel.helpers.EndToEndSpec

class PlayJsonSpec extends EndToEndSpec {
  import PlayJsonSpec._

  "The generator" should "generate the simple json of all defined types when no validation" in {
    val expected = definedTypeSample(false)
    val output = processFile("json.definedType", configuration(false))
    output should beValidRoot(expected)
  }

  it should "generate the json of all defined types with flat validation" in {
    val expected = definedTypeSample(true)
    val output = processFile("json.definedType", configuration(true))
    output should beValidRoot(expected)
  }
}

object PlayJsonSpec extends BaseJsonSpec {

  override protected def jsonFormat = JsonFormat.play

  private val imports = Seq(Import("play.api.libs.json._"), Import("definiti.native.JsonPlaySupport._"))

  override protected def firstDefinedTypeJsonWithValidation = {
    imports ++ Seq(
      rawFormat("MyFirstType"),
      validationFormat("MyFirstType")
    )
  }

  override protected def firstDefinedTypeJsonWithoutValidation = {
    imports :+ simpleFormat("MyFirstType")
  }

  override protected def secondDefinedTypeJsonWithValidation = {
    imports ++ Seq(
      ClassVal(
        name = "MySecondTypeRawFormat",
        typ = "OFormat[MySecondType]",
        body = Seq(
          Val("MyFirstTypeFormat", CallAttribute("MyFirstType", "MyFirstTypeRawFormat"), isImplicit = true),
          Val("MyThirdTypeFormat", CallAttribute("MyThirdType", "MyThirdTypeRawFormat"), isImplicit = true),
          CallAttribute("Json", "format[MySecondType]")
        )
      ),
      validationFormat("MySecondType")
    )
  }

  override protected def secondDefinedTypeJsonWithoutValidation = {
    imports :+ simpleFormat("MySecondType")
  }

  override protected def thirdDefinedTypeJsonWithValidation = {
    imports ++ Seq(
      rawFormat("MyThirdType"),
      validationFormat("MyThirdType")
    )
  }

  override protected def thirdDefinedTypeJsonWithoutValidation = {
    imports :+ simpleFormat("MyThirdType")
  }

  private def simpleFormat(typeName: String) = {
    ClassVal(
      name = s"${typeName}Format",
      typ = s"OFormat[${typeName}]",
      body = Seq(CallAttribute("Json", s"format[${typeName}]")),
      isImplicit = true
    )
  }

  private def rawFormat(typeName: String) = {
    ClassVal(
      name = s"${typeName}RawFormat",
      typ = s"OFormat[${typeName}]",
      body = Seq(CallAttribute("Json", s"format[${typeName}]"))
    )
  }

  private def validationFormat(typeName: String) = {
    ClassVal(
      name = s"${typeName}Format",
      typ = s"OFormat[${typeName}]",
      body = Seq(CallFunction("formatWithValidation", s"${typeName}RawFormat", "allVerifications")),
      isImplicit = true
    )
  }
}