package definiti.scalamodel

import definiti.core.ValidValue
import definiti.scalamodel.ScalaAST._
import definiti.scalamodel.helpers.{ConfigurationMock, EndToEndSpec}

class PlayJsonSpec extends EndToEndSpec {
  import PlayJsonSpec._

  "The generator" should "generate the simple json of all defined types when no validation" in {
    val expected = ValidValue(definedType(JsonValidation.none))
    val output = processFile("json.definedType", configuration(JsonValidation.none))
    output should be(expected)
  }

  it should "generate the json of all defined types with flat validation" in {
    val expected = ValidValue(definedType(JsonValidation.flat))
    val output = processFile("json.definedType", configuration(JsonValidation.flat))
    output should be(expected)
  }
}

object PlayJsonSpec {
  import definiti.scalamodel.helpers.AstHelper._

  def configuration(jsonValidation: JsonValidation.Value): Configuration = {
    ConfigurationMock(
      json = JsonConfiguration(
        format = JsonFormat.play,
        validation = jsonValidation
      )
    )
  }

  def definedType(validation: JsonValidation.Value): Root = {
    Root(
      Package(
        "my",
        CaseClassDef("MyFirstType", Parameter("myAttribute", "String")),
        firstDefinedTypeObject(validation),
        CaseClassDef("MySecondType", Parameter("myFirstAttribute", "BigDecimal"), Parameter("mySecondAttribute", "my.MyFirstType")),
        secondDefinedTypeObject(validation)
      )
    )
  }

  private def firstDefinedTypeObject(validation: JsonValidation.Value): ObjectDef = {
    ObjectDef(
      name = "MyFirstType",
      body = Seq(
        attributeVerification("myAttribute", "String"),
        typeVerifications("MyFirstType"),
        allVerifications("MyFirstType", "myAttribute"),
        applyCheck("MyFirstType", "myAttribute" -> "String")
      ) ++ firstDefinedTypeJson(validation)
    )
  }

  private def firstDefinedTypeJson(validation: JsonValidation.Value): Seq[Statement] = {
    validation match {
      case JsonValidation.none =>
        Seq(
          Import("play.api.libs.json._"),
          Import("definiti.native.JsonPlaySupport._"),
          ClassVal(
            name = s"MyFirstTypeFormat",
            typ = s"OFormat[MyFirstType]",
            body = Seq(CallAttribute("Json", "format[MyFirstType]")),
            isImplicit = true
          )
        )
      case JsonValidation.flat =>
        Seq(
          Import("play.api.libs.json._"),
          Import("definiti.native.JsonPlaySupport._"),
          ClassVal(
            name = s"MyFirstTypeFormat",
            typ = s"OFormat[MyFirstType]",
            body = Seq(
              CallFunction(
                target = "formatWithValidation",
                CallAttribute("Json", "format[MyFirstType]"),
                SimpleExpression("allVerifications")
              )
            ),
            isImplicit = true
          )
        )
    }
  }

  private def secondDefinedTypeObject(validation: JsonValidation.Value): ObjectDef = {
    ObjectDef(
      name = "MySecondType",
      body = Seq(
        attributeVerification("myFirstAttribute", "BigDecimal"),
        attributeVerificationDefinedType("mySecondAttribute", "my.MyFirstType"),
        typeVerifications("MySecondType"),
        allVerifications("MySecondType", "myFirstAttribute", "mySecondAttribute"),
        applyCheck("MySecondType", "myFirstAttribute" -> "BigDecimal", "mySecondAttribute" -> "my.MyFirstType")
      ) ++ secondDefinedTypeJson(validation)
    )
  }

  private def secondDefinedTypeJson(validation: JsonValidation.Value): Seq[Statement] = {
    validation match {
      case JsonValidation.none =>
        Seq(
          Import("play.api.libs.json._"),
          Import("definiti.native.JsonPlaySupport._"),
          ClassVal(
            name = "MySecondTypeFormat",
            typ = "OFormat[MySecondType]",
            body = Seq(CallAttribute("Json", "format[MySecondType]")),
            isImplicit = true
          )
        )
      case JsonValidation.flat =>
        Seq(
          Import("play.api.libs.json._"),
          Import("definiti.native.JsonPlaySupport._"),
          ClassVal(
            name = "MySecondTypeFormat",
            typ = "OFormat[MySecondType]",
            body = Seq(
              CallFunction(
                target = "formatWithValidation",
                CallAttribute("Json", "format[MySecondType]"),
                SimpleExpression("allVerifications")
              )
            ),
            isImplicit = true
          )
        )
    }
  }
}