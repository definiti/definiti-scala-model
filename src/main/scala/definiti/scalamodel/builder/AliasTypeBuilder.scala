package definiti.scalamodel.builder

import definiti.core.ast._
import definiti.scalamodel.ScalaAST
import definiti.scalamodel.utils.{ListUtils, StringUtils}

trait AliasTypeBuilder {
  self: ScalaModelBuilder =>

  def generateAliasType(aliasType: AliasType): Seq[ScalaAST.TopLevelElement] = {
    val commentElement = aliasType.comment.map(comment => ScalaAST.Comment(comment.trim))
    val objectElement = ScalaAST.ObjectDef(
      name = aliasType.name,
      extendz = None,
      body = Seq(generatePublicVerification(aliasType)),
      property = None
    )

    commentElement.toSeq :+ objectElement
  }

  private def generatePublicVerification(aliasType: AliasType): ScalaAST.Statement = {
    val verifications = Seq(
      generateAliasVerification(aliasType).toSeq,
      generateInternalVerificationObjects(aliasType),
      Seq(generateVerification(aliasType))
    ).flatten
    if (aliasType.parameters.nonEmpty || aliasType.genericTypes.nonEmpty) {
      ScalaAST.Def1(
        name = "verification",
        typ = ScalaAST.Type("Verification", generateType(aliasType.alias)).toCode,
        generics = aliasType.genericTypes,
        parameters = aliasType.parameters.map(generateParameter),
        body = Some(ScalaAST.Block(verifications)),
        property = None
      )
    } else {
      ScalaAST.ClassVal(
        name = "verification",
        typ = ScalaAST.Type("Verification", generateType(aliasType.alias)).toCode,
        body = verifications
      )
    }
  }

  private def generateAliasVerification(aliasType: AliasType): Option[ScalaAST.Statement] = {
    library.typesMap.get(aliasType.alias.typeName) match {
      case Some(_: AliasType | _: DefinedType) =>
        if (aliasType.alias.parameters.nonEmpty) {
          Some(ScalaAST.CallMethod(
            target = ScalaAST.SimpleExpression(StringUtils.lastPart(aliasType.alias.typeName)),
            name = s"verification${generateGenericTypes(aliasType.alias.genericTypes)}",
            arguments = aliasType.alias.parameters.map(generateExpression)
          ))
        } else {
          Some(ScalaAST.CallAttribute(
            target = ScalaAST.SimpleExpression(StringUtils.lastPart(aliasType.alias.typeName)),
            name = s"verification${generateGenericTypes(aliasType.alias.genericTypes)}"
          ))
        }
      case _ => None
    }
  }

  private def generateVerification(aliasType: AliasType): ScalaAST.Statement = {
    val verifications = generateInheritedVerifications(aliasType) ++ generateInternalVerifications(aliasType)
    ScalaAST.CallMethod(
      "Verification",
      "all",
      verifications: _*
    )
  }

  private def generateInheritedVerifications(aliasType: AliasType): Seq[ScalaAST.Expression] = {
    verificationsFromType(aliasType)
      .map(generateVerificationCall(_, aliasType))
  }

  private def generateVerificationCall(verificationReference: VerificationReference, aliasType: AliasType): ScalaAST.Expression = {
    val verification = library.verificationsMap(verificationReference.verificationName)
    ScalaAST.New(
      name = verificationReference.verificationName,
      generics = ListUtils.replaceOrdered(verification.function.genericTypes, aliasType.genericTypes),
      arguments = verificationReference.parameters.map(generateExpression)
    )
  }

  private def generateInternalVerifications(aliasType: AliasType): Seq[ScalaAST.Expression] = {
    aliasType.verifications.indices.map { index =>
      ScalaAST.SimpleExpression(s"${aliasType.name}${index}")
    }
  }

  private def generateInternalVerificationObjects(aliasType: AliasType): Seq[ScalaAST.Statement] = {
    aliasType.verifications.zipWithIndex.map { case (typeVerification, index) =>
      generateTypeVerificationObject(s"${aliasType.name}${index}", typeVerification)
    }
  }
}
