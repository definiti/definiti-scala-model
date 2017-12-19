package definiti.scalamodel

import definiti.scalamodel.ScalaAST._
import definiti.scalamodel.helpers.{ConfigurationMock, EndToEndSpec}

class PlayJsonSpec extends EndToEndSpec {
  import PlayJsonSpec._

  "The generator" should "generate the simple json of all defined types when no validation" in {
    val expected = definedType(false)
    val output = processFile("json.definedType", configuration(false))
    output should beValidRoot(expected)
  }

  it should "generate the json of all defined types with flat validation" in {
    val expected = definedType(true)
    val output = processFile("json.definedType", configuration(true))
    output should beValidRoot(expected)
  }
}

object PlayJsonSpec {
  import definiti.scalamodel.helpers.AstHelper._

  def configuration(jsonValidation: Boolean): Configuration = {
    ConfigurationMock(
      json = JsonConfiguration(
        format = JsonFormat.play,
        validation = jsonValidation
      )
    )
  }

  def definedType(validation: Boolean): Root = {
    Root(
      Package(
        "my",
        Seq.empty,
        CaseClassDef("MyFirstType", Parameter("myAttribute", "String")),
        firstDefinedTypeObject(validation),
        CaseClassDef(
          "MySecondType",
          Parameter("myFirstAttribute", "BigDecimal"),
          Parameter("mySecondAttribute", "MyFirstType"),
          Parameter("myThirdAttribute", "MyFirstType")
        ),
        secondDefinedTypeObject(validation)
      )
    )
  }

  private def firstDefinedTypeObject(validation: Boolean): ObjectDef = {
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

  private def firstDefinedTypeJson(validation: Boolean): Seq[Statement] = {
    if (validation) {
      Seq(
        Import("play.api.libs.json._"),
        Import("definiti.native.JsonPlaySupport._"),
        ClassVal(
          name = s"MyFirstTypeRawFormat",
          typ = s"OFormat[MyFirstType]",
          body = Seq(CallAttribute("Json", "format[MyFirstType]"))
        ),
        ClassVal(
          name = s"MyFirstTypeFormat",
          typ = s"OFormat[MyFirstType]",
          body = Seq(
            CallFunction(
              target = "formatWithValidation",
              SimpleExpression("MyFirstTypeRawFormat"),
              SimpleExpression("allVerifications")
            )
          ),
          isImplicit = true
        )
      )
    } else {
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
    }
  }

  private def secondDefinedTypeObject(validation: Boolean): ObjectDef = {
    ObjectDef(
      name = "MySecondType",
      body = Seq(
        attributeVerification("myFirstAttribute", "BigDecimal"),
        attributeVerificationDefinedType("mySecondAttribute", "MyFirstType"),
        attributeVerificationDefinedType("myThirdAttribute", "MyFirstType"),
        typeVerifications("MySecondType"),
        allVerifications("MySecondType", "myFirstAttribute", "mySecondAttribute", "myThirdAttribute"),
        applyCheck("MySecondType", "myFirstAttribute" -> "BigDecimal", "mySecondAttribute" -> "MyFirstType", "myThirdAttribute" -> "MyFirstType")
      ) ++ secondDefinedTypeJson(validation)
    )
  }

  private def secondDefinedTypeJson(validation: Boolean): Seq[Statement] = {
    if (validation) {
      Seq(
        Import("play.api.libs.json._"),
        Import("definiti.native.JsonPlaySupport._"),
        ClassVal(
          name = "MySecondTypeRawFormat",
          typ = "OFormat[MySecondType]",
          body = Seq(
            Val("MyFirstTypeFormat", CallAttribute("MyFirstType", "MyFirstTypeRawFormat"), isImplicit = true),
            CallAttribute("Json", "format[MySecondType]")
          )
        ),
        ClassVal(
          name = "MySecondTypeFormat",
          typ = "OFormat[MySecondType]",
          body = Seq(CallFunction("formatWithValidation", "MySecondTypeRawFormat", "allVerifications")),
          isImplicit = true
        )
      )
    } else {
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
    }
  }
}