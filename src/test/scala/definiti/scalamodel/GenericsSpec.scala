package definiti.scalamodel

import definiti.scalamodel.helpers.EndToEndSpec

class GenericsSpec extends EndToEndSpec {
  "The generator" should "generate a valid scala AST for an alias type with generics and verification reference" in {
    processSample("generics.aliasWithVerification")
  }

  it should "use alias type verification with generic" in {
    processSample("generics.attributeAsAliasTypeWithGeneric")
  }

  it should "generate attribute verifications with generics" in {
    processSample("generics.attributeVerificationWithGeneric")
  }
}