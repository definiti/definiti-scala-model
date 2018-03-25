package definiti.scalamodel

import definiti.scalamodel.helpers.EndToEndSpec

class SpecificSpec extends EndToEndSpec {
  "The generator" should "force the usage of BigDecimal for all numbers" in {
    processSample("specific.numberToBigDecimal")
  }

  it should "use alias type verification and not duplicate them" in {
    processSample("specific.attributeAsAliasType")
  }
}