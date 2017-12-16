package definiti.scalamodel

import definiti.core.ValidValue
import definiti.scalamodel.ScalaAST._
import definiti.scalamodel.helpers.EndToEndSpec

class SpecificSpec extends EndToEndSpec {
  import SpecificSpec._

  "The generator" should "force the usage of BigDecimal for all numbers" in {
    val expected = ValidValue(numberToBigDecimalResult)
    val output = processFile("specific.numberToBigDecimal")
    output should be(expected)
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
}