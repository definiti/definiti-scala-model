package definiti.scalamodel

import definiti.scalamodel.ScalaAST._
import definiti.scalamodel.helpers.{ConfigurationMock, EndToEndSpec}

class SprayJsonSpec extends EndToEndSpec {
  import SprayJsonSpec._

  "The generator" should "generate the simple json of all defined type when no validation" in {
    val expected = definedType(false)
    val output = processFile("json.definedType", configuration(false))
    output should beValidRoot(expected)
  }

  it should "generate the json of all defined type with flat validation" in {
    val expected = definedType(true)
    val output = processFile("json.definedType", configuration(true))
    output should beValidRoot(expected)
  }
}

object SprayJsonSpec {
  import definiti.scalamodel.helpers.AstHelper._

  def configuration(jsonValidation: Boolean): Configuration = {
    ConfigurationMock(
      json = JsonConfiguration(
        format = JsonFormat.spray,
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
          Parameter("myThirdAttribute", "List[MyFirstType]"),
          Parameter("myFourthAttribute", "Option[MyFirstType]")
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
        Import("spray.json.RootJsonFormat"),
        Import("definiti.native.JsonSpraySupport._"),
        ClassVal(
          name = s"MyFirstTypeRawFormat",
          typ = s"RootJsonFormat[MyFirstType]",
          body = Seq(CallFunction(
            target = "jsonFormat1",
            SimpleExpression(s"MyFirstType.apply")
          ))
        ),
        ClassVal(
          name = s"MyFirstTypeFormat",
          typ = s"RootJsonFormat[MyFirstType]",
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
        Import("spray.json.RootJsonFormat"),
        Import("definiti.native.JsonSpraySupport._"),
        ClassVal(
          name = s"MyFirstTypeFormat",
          typ = s"RootJsonFormat[MyFirstType]",
          body = Seq(CallFunction(s"jsonFormat1", SimpleExpression(s"MyFirstType.apply"))),
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
        attributeVerificationDefinedType("myThirdAttribute", "MyFirstType", "List"),
        attributeVerificationDefinedType("myFourthAttribute", "MyFirstType", "Option"),
        typeVerifications("MySecondType"),
        allVerifications("MySecondType", "myFirstAttribute", "mySecondAttribute", "myThirdAttribute", "myFourthAttribute"),
        applyCheck(
          "MySecondType",
          "myFirstAttribute" -> "BigDecimal",
          "mySecondAttribute" -> "MyFirstType",
          "myThirdAttribute" -> "List[MyFirstType]",
          "myFourthAttribute" -> "Option[MyFirstType]"
        )
      ) ++ secondDefinedTypeJson(validation)
    )
  }

  private def secondDefinedTypeJson(validation: Boolean): Seq[Statement] = {
    if (validation) {
      Seq(
        Import("spray.json.RootJsonFormat"),
        Import("definiti.native.JsonSpraySupport._"),
        ClassVal(
          name = s"MySecondTypeRawFormat",
          typ = s"RootJsonFormat[MySecondType]",
          body = Seq(
            Val(
              name = "MyFirstTypeFormat",
              value = CallAttribute("MyFirstType", "MyFirstTypeRawFormat"),
              isImplicit = true
            ),
            CallFunction(
              target = "jsonFormat4",
              SimpleExpression(s"MySecondType.apply")
            )
          )
        ),
        ClassVal(
          name = s"MySecondTypeFormat",
          typ = s"RootJsonFormat[MySecondType]",
          body = Seq(
            CallFunction(
              target = "formatWithValidation",
              SimpleExpression(s"MySecondTypeRawFormat"),
              SimpleExpression("allVerifications")
            )
          ),
          isImplicit = true
        )
      )
    } else {
      Seq(
        Import("spray.json.RootJsonFormat"),
        Import("definiti.native.JsonSpraySupport._"),
        ClassVal(
          name = s"MySecondTypeFormat",
          typ = s"RootJsonFormat[MySecondType]",
          body = Seq(CallFunction(s"jsonFormat4", SimpleExpression(s"MySecondType.apply"))),
          isImplicit = true
        )
      )
    }
  }
}