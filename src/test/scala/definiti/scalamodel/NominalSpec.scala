package definiti.scalamodel

import definiti.scalamodel.ScalaAST._
import definiti.scalamodel.helpers.EndToEndSpec

class NominalSpec extends EndToEndSpec {
  import NominalSpec._

  "The generator" should "generate a valid scala AST for a valid defined type" in {
    val output = processFile("nominal.definedType")
    output should beValidRoot(definedType)
  }

  it should "generate a valid scala AST for a valid alias type" in {
    val output = processFile("nominal.aliasType")
    output should beValidRoot(aliasType)
  }

  it should "generate a valid scala AST for a valid verification" in {
    val output = processFile("nominal.verification")
    output should beValidRoot(verification)
  }

  it should "generate a valid scala AST for a valid named function" in {
    val output = processFile("nominal.namedFunction")
    output should beValidRoot(namedFunction)
  }

  it should "generate nothing from an extended context" in {
    val output = processFile("nominal.extendedContext")
    output should beValidRoot(extendedContext)
  }

  it should "generate a valid scala AST for a valid alias type in a package" in {
    val output = processFile("nominal.package")
    output should beValidRoot(packageAliasType)
  }
}


object NominalSpec {
  import definiti.scalamodel.helpers.AstHelper._

  val definedType: Root = Root(
    CaseClassDef("MyType", Parameter("myAttribute", "String")),
    ObjectDef(
      name = "MyType",
      body = Seq(
        attributeVerification("myAttribute", "String"),
        typeVerifications("MyType"),
        allVerifications("MyType", "myAttribute"),
        applyCheck("MyType", "myAttribute" -> "String")
      )
    )
  )

  val aliasType: Root = Root(
    ObjectDef(
      name = "AliasString",
      body = Seq(
        typeVerifications("AliasString", "String"),
        aliasApplyCheck("AliasString", "String")
      )
    )
  )

  val verification: Root = Root(
    verificationObject("AlwaysTrue", "String", "Never fail", SimpleExpression("true"))
  )

  val namedFunction: Root = Root(
    Def1("alwaysFalse", "Boolean", Seq.empty, SimpleExpression("false"))
  )

  val extendedContext: Root = Root()

  val packageAliasType: Root = Root(
    Package(
      "tst",
      ObjectDef(
        name = "AliasString",
        body = Seq(
          typeVerifications("AliasString", "String"),
          aliasApplyCheck("AliasString", "String")
        )
      )
    )
  )
}