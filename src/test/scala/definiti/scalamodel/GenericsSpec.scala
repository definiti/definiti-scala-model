package definiti.scalamodel

import definiti.scalamodel.helpers.EndToEndSpec

class GenericsSpec extends EndToEndSpec {
  "The generator" should "generate a valid scala AST for an alias type with generics and verification reference" in {
    processSample("generics.aliasWithVerification")
  }
}