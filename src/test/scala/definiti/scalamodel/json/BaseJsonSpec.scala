package definiti.scalamodel.json

import definiti.scalamodel.ScalaAST._
import definiti.scalamodel.helpers.AstHelper._
import definiti.scalamodel.helpers.ConfigurationMock
import definiti.scalamodel.{Configuration, JsonConfiguration, JsonFormat}

trait BaseJsonSpec {
  protected def jsonFormat: JsonFormat.Value

  def configuration(jsonValidation: Boolean): Configuration = {
    ConfigurationMock(
      json = JsonConfiguration(
        format = jsonFormat,
        validation = jsonValidation
      )
    )
  }

  def definedTypeSample(validation: Boolean): Root = {
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
          Parameter("myFourthAttribute", "Option[MyFirstType]"),
          Parameter("myFifthAttribute", "List[MyThirdType]"),
          Parameter("mySixthAttribute", "List[MyThirdType]")
        ),
        secondDefinedTypeObject(validation),
        CaseClassDef("MyThirdType", Parameter("myAttribute", "String")),
        thirdDefinedTypeObject(validation),
        aliasList,
        listOfThird
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
      firstDefinedTypeJsonWithValidation
    } else {
      firstDefinedTypeJsonWithoutValidation
    }
  }

  protected def firstDefinedTypeJsonWithValidation: Seq[Statement]

  protected def firstDefinedTypeJsonWithoutValidation: Seq[Statement]

  private def secondDefinedTypeObject(validation: Boolean): ObjectDef = {
    ObjectDef(
      name = "MySecondType",
      body = Seq(
        attributeVerification("myFirstAttribute", "BigDecimal"),
        attributeVerificationDefinedType("mySecondAttribute", "MyFirstType"),
        attributeVerificationDefinedType("myThirdAttribute", "MyFirstType", "List"),
        attributeVerificationDefinedType("myFourthAttribute", "MyFirstType", "Option"),
        ClassVal(
          name = s"myFifthAttributeVerification",
          typ = s"Verification[List[MyThirdType]]",
          body = CallMethod(
            "Verification",
            "all",
            CallAttribute("AliasList", "AliasListVerifications[MyThirdType]"),
            New("ListVerification", CallAttribute("MyThirdType", "allVerifications"))
          )
        ),
        attributeVerificationAliasType("mySixthAttribute", "ListOfThird", "List[MyThirdType]"),
        typeVerifications("MySecondType"),
        allVerifications("MySecondType", "myFirstAttribute", "mySecondAttribute", "myThirdAttribute", "myFourthAttribute", "myFifthAttribute", "mySixthAttribute"),
        applyCheck(
          "MySecondType",
          "myFirstAttribute" -> "BigDecimal",
          "mySecondAttribute" -> "MyFirstType",
          "myThirdAttribute" -> "List[MyFirstType]",
          "myFourthAttribute" -> "Option[MyFirstType]",
          "myFifthAttribute" -> "List[MyThirdType]",
          "mySixthAttribute" -> "List[MyThirdType]"
        )
      ) ++ secondDefinedTypeJson(validation)
    )
  }

  private def secondDefinedTypeJson(validation: Boolean): Seq[Statement] = {
    if (validation) {
      secondDefinedTypeJsonWithValidation
    } else {
      secondDefinedTypeJsonWithoutValidation
    }
  }

  protected def secondDefinedTypeJsonWithValidation: Seq[Statement]

  protected def secondDefinedTypeJsonWithoutValidation: Seq[Statement]

  private def thirdDefinedTypeObject(validation: Boolean): ObjectDef = {
    ObjectDef(
      name = "MyThirdType",
      body = Seq(
        attributeVerification("myAttribute", "String"),
        typeVerifications("MyThirdType"),
        allVerifications("MyThirdType", "myAttribute"),
        applyCheck("MyThirdType", "myAttribute" -> "String")
      ) ++ thirdDefinedTypeJson(validation)
    )
  }

  private def thirdDefinedTypeJson(validation: Boolean): Seq[Statement] = {
    if (validation) {
      thirdDefinedTypeJsonWithValidation
    } else {
      thirdDefinedTypeJsonWithoutValidation
    }
  }

  protected def thirdDefinedTypeJsonWithValidation: Seq[Statement]

  protected def thirdDefinedTypeJsonWithoutValidation: Seq[Statement]

  private def aliasList: ObjectDef = {
    ObjectDef(
      "AliasList",
      Def0(
        name = "AliasListVerifications",
        typ = "Verification[List[A]]",
        generics = Seq("A"),
        body = Some(SimpleExpression("Verification.none[List[A]]"))
      ),
      Def1(
        name = "applyCheck[A]",
        typ = "Validation[List[A]]",
        generics = Seq.empty,
        parameters = Seq(Parameter("input", "List[A]")),
        body = Some(CallMethod("AliasListVerifications[A]", "verify", "input"))
      )
    )
  }

  private def listOfThird: ObjectDef = {
    ObjectDef(
      "ListOfThird",
      ClassVal(
        name = "ListOfThirdVerifications",
        typ = "Verification[List[MyThirdType]]",
        body = SimpleExpression("Verification.none[List[MyThirdType]]")
      ),
      Def1(
        name = "applyCheck",
        typ = "Validation[List[MyThirdType]]",
        generics = Seq.empty,
        parameters = Seq(Parameter("input", "List[MyThirdType]")),
        body = Some(CallMethod("ListOfThirdVerifications", "verify", "input"))
      )
    )
  }
}
