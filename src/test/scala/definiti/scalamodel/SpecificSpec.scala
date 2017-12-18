package definiti.scalamodel

import definiti.scalamodel.ScalaAST.{ObjectDef, _}
import definiti.scalamodel.helpers.AstHelper._
import definiti.scalamodel.helpers.EndToEndSpec

class SpecificSpec extends EndToEndSpec {
  import SpecificSpec._

  "The generator" should "force the usage of BigDecimal for all numbers" in {
    val output = processFile("specific.numberToBigDecimal")
    output should beValidRoot(numberToBigDecimalResult)
  }

  it should "use alias type verification and not duplicate them" in {
    val output = processFile("specific.attributeAsAliasType")
    output should beValidRoot(attributeAsAliasType)
  }
}

object SpecificSpec {
  val numberToBigDecimalResult: Root = Root(
    Package(
      "numbers",
      Def1(
        name = "twice",
        typ = "BigDecimal",
        parameters = Seq(Parameter("n", "BigDecimal")),
        body = BinaryOp("+", SimpleExpression("n"), SimpleExpression("n"))
      ),
      Def1(
        name = "sum",
        typ = "BigDecimal",
        parameters = Seq(Parameter("values", "List[BigDecimal]")),
        body = CallFunction(
          "ListExtension.foldLeft",
          SimpleExpression("values"),
          CallFunction("BigDecimal", SimpleExpression("0")),
          Lambda(
            parameters = Seq(Parameter("acc", "BigDecimal"), Parameter("current", "BigDecimal")),
            body = BinaryOp("+", SimpleExpression("acc"), SimpleExpression("current"))
          )
        )
      )
    )
  )

  val attributeAsAliasType: Root = Root(
    Package(
      "my",
      verificationObject("IsRequired", "String", "This string is required", CallFunction("StringExtension.nonEmpty", SimpleExpression("string")), "string"),
      CaseClassDef("MyType", Parameter("attribute", "String")),
      ObjectDef(
        name = "MyType",
        body = Seq(
          attributeVerificationAliasType("attribute", "my.RequiredString", "String"),
          typeVerifications("MyType"),
          allVerifications("MyType", "attribute"),
          applyCheck("MyType", "attribute" -> "String")
        )
      ),
      ObjectDef(
        name = "RequiredString",
        body = Seq(
          ClassVal(
            name = "RequiredStringVerifications",
            typ = "Verification[String]",
            body = Seq(CallFunction("my.IsRequired"))
          ),
          aliasApplyCheck("RequiredString", "String")
        )
      )
    )
  )
}