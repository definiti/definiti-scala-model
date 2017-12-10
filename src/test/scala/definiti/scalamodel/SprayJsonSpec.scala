package definiti.scalamodel

import definiti.core.ValidValue
import definiti.scalamodel.ScalaAST._
import definiti.scalamodel.helpers.{ConfigurationMock, EndToEndSpec}

class SprayJsonSpec extends EndToEndSpec {
  import SprayJsonSpec._

  "The generator" should "generate the simple json of all defined type when no validation" in {
    val expected = ValidValue(definedType(JsonValidation.none))
    val output = processFile("json.definedType", configuration(JsonValidation.none))
    output should be(expected)
  }

  it should "generate the json of all defined type with flat validation" in {
    val expected = ValidValue(definedType(JsonValidation.flat))
    val output = processFile("json.definedType", configuration(JsonValidation.flat))
    output should be(expected)
  }
}

object SprayJsonSpec {
  import definiti.scalamodel.helpers.AstHelper._

  def configuration(jsonValidation: JsonValidation.Value): Configuration = {
    ConfigurationMock(
      json = JsonConfiguration(
        format = JsonFormat.spray,
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
          Import("spray.json.RootJsonFormat"),
          Import("definiti.native.JsonSpraySupport._"),
          ClassVal(
            name = s"MyFirstTypeFormat",
            typ = s"RootJsonFormat[MyFirstType]",
            body = Seq(CallFunction(s"jsonFormat1", SimpleExpression(s"MyFirstType.apply"))),
            isImplicit = true
          )
        )
      case JsonValidation.flat =>
        Seq(
          Import("spray.json.RootJsonFormat"),
          Import("definiti.native.JsonSpraySupport._"),
          ClassVal(
            name = s"MyFirstTypeFormat",
            typ = s"RootJsonFormat[MyFirstType]",
            body = Seq(
              CallFunction(
                target = "formatWithValidation",
                CallFunction(
                  target = "jsonFormat1",
                  SimpleExpression(s"MyFirstType.apply")
                ),
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
          Import("spray.json.RootJsonFormat"),
          Import("definiti.native.JsonSpraySupport._"),
          ClassVal(
            name = s"MySecondTypeFormat",
            typ = s"RootJsonFormat[MySecondType]",
            body = Seq(CallFunction(s"jsonFormat2", SimpleExpression(s"MySecondType.apply"))),
            isImplicit = true
          )
        )
      case JsonValidation.flat =>
        Seq(
          Import("spray.json.RootJsonFormat"),
          Import("definiti.native.JsonSpraySupport._"),
          ClassVal(
            name = s"MySecondTypeFormat",
            typ = s"RootJsonFormat[MySecondType]",
            body = Seq(
              CallFunction(
                target = "formatWithValidation",
                CallFunction(
                  target = "jsonFormat2",
                  SimpleExpression(s"MySecondType.apply")
                ),
                SimpleExpression("allVerifications")
              )
            ),
            isImplicit = true
          )
        )
    }
  }
}