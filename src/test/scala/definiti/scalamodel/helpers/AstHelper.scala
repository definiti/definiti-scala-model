package definiti.scalamodel.helpers

import definiti.scalamodel.ScalaAST._
import definiti.scalamodel.utils.StringUtils

object AstHelper {
  def attributeVerification(name: String, typ: String): ClassVal = {
    ClassVal(s"${name}Verification", s"Verification[${typ}]", SimpleExpression(s"Verification.none[${typ}]"))
  }

  def attributeVerificationDefinedType(name: String, typ: String): ClassVal = {
    ClassVal(
      name = s"${name}Verification",
      typ = s"Verification[${typ}]",
      body = CallAttribute(SimpleExpression(typ), "allVerifications")
    )
  }

  def attributeVerificationAliasType(name: String, typ: String, realType: String): ClassVal = {
    ClassVal(
      name = s"${name}Verification",
      typ = s"Verification[${realType}]",
      body = CallAttribute(SimpleExpression(typ), s"${StringUtils.lastPart(typ, '.')}Verifications")
    )
  }

  def typeVerifications(typ: String): ClassVal = {
    typeVerifications(typ, typ)
  }

  def typeVerifications(typ: String, realType: String): ClassVal = {
    ClassVal(s"${typ}Verifications", s"Verification[${realType}]", SimpleExpression(s"Verification.none[${realType}]"))
  }

  def allVerifications(typ: String, attribute: String): ClassVal = {
    ClassVal(
      name = "allVerifications",
      typ = s"Verification[${typ}]",
      body = Seq(
        CallMethod(
          target = CallMethod(
            target = SimpleExpression(s"${attribute}Verification"),
            name = "from",
            arguments = Seq(
              Lambda(
                parameters = Seq(Parameter("x", typ)),
                body = CallAttribute(SimpleExpression("x"), attribute)
              ),
              StringExpression(attribute)
            )
          ),
          name = "andThen",
          arguments = Seq(SimpleExpression(s"${typ}Verifications"))
        )
      )
    )
  }

  def allVerifications(typ: String, firstAttribute: String, otherAttributes: String*): ClassVal = {
    val attributes = firstAttribute +: otherAttributes
    ClassVal(
      name = "allVerifications",
      typ = s"Verification[${typ}]",
      body = Seq(
        CallMethod(
          target = CallMethod(
            target = SimpleExpression("Verification"),
            name = "all",
            arguments = attributes.map { attribute =>
              CallMethod(
                target = SimpleExpression(s"${attribute}Verification"),
                name = "from",
                arguments = Seq(
                  Lambda(
                    parameters = Seq(Parameter("x", typ)),
                    body = CallAttribute(SimpleExpression("x"), attribute)
                  ),
                  StringExpression(attribute)
                )
              )
            }
          ),
          name = "andThen",
          arguments = Seq(SimpleExpression(s"${typ}Verifications"))
        )
      )
    )
  }

  def applyCheck(typ: String, attributes: (String, String)*): Def1 = {
    Def1(
      name = "applyCheck",
      typ = s"Validation[${typ}]",
      generics = Seq.empty,
      parameters = attributes.map(attribute => Parameter(attribute._1, attribute._2)),
      body = Some(CallMethod(
        target = SimpleExpression("allVerifications"),
        name = "verify",
        arguments = Seq(
          CallFunction(
            target = SimpleExpression(typ),
            arguments = attributes.map(attribute => SimpleExpression(attribute._1))
          )
        )
      )),
      property = None
    )
  }

  def aliasApplyCheck(typ: String, realType: String): Def1 = {
    Def1(
      name = "applyCheck",
      typ = s"Validation[${realType}]",
      generics = Seq.empty,
      parameters = Seq(Parameter("input", realType)),
      body = Some(CallMethod(s"${typ}Verifications", "verify", SimpleExpression("input"))),
      None
    )
  }

  def verificationObject(name: String, typ: String, message: String, body: Expression, attributeName: String = "x"): ObjectDef = {
    ObjectDef(
      name = name,
      body = Seq(
        Def1(
          name = "apply",
          typ = s"Verification[${typ}]",
          generics = Seq.empty,
          parameters = Seq(Parameter("message", typ, StringExpression(message))),
          body = Some(CallFunction(
            target = CallFunction(
              target = "Verification",
              arguments = SimpleExpression("message")
            ),
            arguments = Seq(
              Lambda(
                parameters = Seq(Parameter(attributeName, typ)),
                body = body
              )
            )
          )),
          None
        )
      )
    )
  }
}
