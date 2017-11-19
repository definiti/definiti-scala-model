package definiti.scalamodel

import definiti.core.ValidValue
import definiti.scalamodel.ScalaAST._
import definiti.scalamodel.helpers.EndToEndSpec

class NominalSpec extends EndToEndSpec {
  import NominalSpec._

  "The generator" should "generate a valid scala AST for a valid defined type" in {
    val expected = ValidValue(definedType)
    val output = processFile("nominal.definedType")
    output should be(expected)
  }

  it should "generate a valid scala AST for a valid alias type" in {
    val expected = ValidValue(aliasType)
    val output = processFile("nominal.aliasType")
    output should be(expected)
  }

  it should "generate a valid scala AST for a valid verification" in {
    val expected = ValidValue(verification)
    val output = processFile("nominal.verification")
    output should be(expected)
  }

  it should "generate a valid scala AST for a valid named function" in {
    val expected = ValidValue(namedFunction)
    val output = processFile("nominal.namedFunction")
    output should be(expected)
  }

  it should "generate nothing from an extended context" in {
    val expected = ValidValue(extendedContext)
    val output = processFile("nominal.extendedContext")
    output should be(expected)
  }

  it should "generate a valid scala AST for a valid alias type in a package" in {
    val expected = ValidValue(packageAliasType)
    val output = processFile("nominal.package")
    output should be(expected)
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