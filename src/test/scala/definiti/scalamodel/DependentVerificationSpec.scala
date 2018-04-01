package definiti.scalamodel

import definiti.scalamodel.helpers.EndToEndSpec

class DependentVerificationSpec extends EndToEndSpec {
  "The generator" should "generate a nominal dependent verification" in {
    processSample("specific.dependentVerification.nominal")
  }

  it should "generate an atomic verification with a dependent verification" in {
    processSample("specific.dependentVerification.atomicWithDependent")
  }

  it should "generate a dependent verification with a child" in {
    processSample("specific.dependentVerification.child")
  }

  it should "generate a dependent verification with a child and grand child" in {
    processSample("specific.dependentVerification.grandChild")
  }

  it should "generate a dependent verification for children even though there is no verification on parent" in {
    processSample("specific.dependentVerification.traverse")
  }

  it should "generate a dependent verification with aggregated parameters" in {
    processSample("specific.dependentVerification.parameterAggregation")
  }

  it should "generate a as different dependent verifications as there is different names" in {
    processSample("specific.dependentVerification.split")
  }
}
