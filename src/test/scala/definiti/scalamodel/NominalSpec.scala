package definiti.scalamodel

import definiti.scalamodel.ScalaAST._
import definiti.scalamodel.helpers.EndToEndSpec

class NominalSpec extends EndToEndSpec {
  import NominalSpec._

  "The generator" should "generate a valid scala AST for a valid defined type" in {
    processSample("nominal.definedType")
  }

  it should "generate a valid scala AST for a valid alias type" in {
    processSample("nominal.aliasType")
  }

  it should "generate a valid scala AST for a valid verification" in {
    processSample("nominal.verification")
  }

  it should "generate a valid scala AST for a valid named function" in {
    processSample("nominal.namedFunction")
  }

  it should "generate nothing from an extended context" in {
    val output = processFile("nominal.extendedContext.input")
    output should beValidRoot(extendedContext)
  }

  it should "generate a valid scala AST for a valid alias type in a package" in {
    processSample("nominal.package")
  }
}


object NominalSpec {
  val extendedContext: Root = Root(Seq.empty)
}