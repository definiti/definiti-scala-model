package definiti.scalamodel.builder.typeVerification

import definiti.core.ast._
import definiti.scalamodel.ScalaAST
import definiti.scalamodel.builder.ScalaModelBuilder
import definiti.scalamodel.model.AliasOrDefinedType
import definiti.scalamodel.utils.StringUtils

trait DependentVerificationBuilder {
  self: ScalaModelBuilder =>

  def generatePublicDependentVerifications(aliasOrDefinedType: AliasOrDefinedType): Seq[ScalaAST.Statement] = {
    extractDependentTypeVerificationNames(aliasOrDefinedType)
      .map(generatePublicDependentVerification(aliasOrDefinedType, _))
  }

  private def generatePublicDependentVerification(aliasOrDefinedType: AliasOrDefinedType, name: String): ScalaAST.Statement = {
    val typ = ScalaAST.Type("Verification", generateType(aliasOrDefinedType))
    val body = ScalaAST.Block(
      generateInternalVerificationObjects(aliasOrDefinedType, name) ++
        generateVerification(aliasOrDefinedType, name)
    ).simplify
    if (aliasOrDefinedType.parameters.nonEmpty || aliasOrDefinedType.genericTypes.nonEmpty) {
      ScalaAST.Def2(
        name = name,
        typ = typ,
        generics = aliasOrDefinedType.genericTypes,
        parameters1 = aliasOrDefinedType.parameters.map(generateParameter),
        parameters2 = extractDependentTypeParameters(aliasOrDefinedType, name),
        body = body,
        property = None
      )
    } else {
      ScalaAST.Def1(
        name = name,
        typ = typ,
        generics = Seq.empty,
        parameters = extractDependentTypeParameters(aliasOrDefinedType, name),
        body = body,
        property = None
      )
    }
  }

  private def generateInternalVerificationObjects(aliasOrDefinedType: AliasOrDefinedType, name: String): Seq[ScalaAST.Statement] = {
    aliasOrDefinedType.verifications
      .collect { case (v: DependentTypeVerification) if v.name == name => v }
      .zipWithIndex
      .map { case (typeVerification, index) =>
        generateDependentTypeVerificationObject(s"${aliasOrDefinedType.name}${index}", typeVerification)
      }
  }

  private def generateVerification(aliasOrDefinedType: AliasOrDefinedType, name: String): ScalaAST.Statement = {
    val attributeVerifications = aliasOrDefinedType.internal match {
      case definedType: DefinedType => definedType.attributes.flatMap(generateAttributeVerifications(_, definedType, name))
      case _ => Seq.empty
    }
    val internalVerifications = generateInternalVerifications(aliasOrDefinedType, name)
    val verifications = attributeVerifications ++ internalVerifications
    ScalaAST.CallMethod("Verification", "all", verifications)
  }

  private def generateAttributeVerifications(attributeDefinition: AttributeDefinition, definedType: DefinedType, name: String): Option[ScalaAST.Expression] = {
    val typeVerification = generateTypeVerificationCall(attributeDefinition.typeDeclaration, name)
    val deepVerification = generateDeepVerification(attributeDefinition.typeDeclaration, name)
    val verifications = typeVerification ++ deepVerification
    val groupVerificationOpt = generateGroupVerification(attributeDefinition.typeDeclaration, verifications)
    groupVerificationOpt.map { groupVerification =>
      ScalaAST.CallMethod(
        target = groupVerification,
        name = s"from",
        arguments = Seq(
          ScalaAST.CallAttribute("_", attributeDefinition.name),
          ScalaAST.StringExpression(attributeDefinition.name)
        ),
        generics = generateType(definedType)
      )
    }
  }

  private def generateTypeVerificationCall(typeDeclaration: TypeDeclaration, name: String): Option[ScalaAST.Expression] = {
    library.typesMap.get(typeDeclaration.typeName) match {
      case Some(x @ (_: AliasType | _: DefinedType)) if hasDependentTypeVerification(x, name) =>
        if (typeDeclaration.parameters.nonEmpty) {
          Some(ScalaAST.CallFunction(
            target = ScalaAST.CallMethod(
              target = StringUtils.lastPart(typeDeclaration.typeName),
              name = name,
              arguments = typeDeclaration.parameters.map(generateExpression),
              generics = typeDeclaration.genericTypes.map(generateType)
            ),
            arguments = extractDependentTypeParameters(x, name).map(_.name).map(ScalaAST.SimpleExpression)
          ))
        } else {
          Some(ScalaAST.CallMethod(
            target = StringUtils.lastPart(typeDeclaration.typeName),
            name = name,
            arguments = extractDependentTypeParameters(x, name).map(_.name).map(ScalaAST.SimpleExpression),
            generics = typeDeclaration.genericTypes.map(generateType)
          ))
        }
      case _ => None
    }
  }

  private def generateDeepVerification(typeDeclaration: TypeDeclaration, name: String): Option[ScalaAST.Expression] = {
    val innerVerification = typeDeclaration.genericTypes.flatMap(generateTypeVerificationCall(_, name))
    if (innerVerification.nonEmpty) {
      if (isList(typeDeclaration)) {
        ScalaAST.New(s"ListVerification", Seq.empty, innerVerification)
      } else if (isOption(typeDeclaration)) {
        ScalaAST.New(s"OptionVerification", Seq.empty, innerVerification)
      } else {
        None
      }
    } else {
      None
    }
  }

  private def generateInternalVerifications(aliasOrDefinedType: AliasOrDefinedType, name: String): Seq[ScalaAST.Expression] = {
    aliasOrDefinedType.verifications
      .collect { case (v: DependentTypeVerification) if v.name == name => v }
      .indices
      .map { index => ScalaAST.SimpleExpression(s"${aliasOrDefinedType.name}${index}") }
  }
}
