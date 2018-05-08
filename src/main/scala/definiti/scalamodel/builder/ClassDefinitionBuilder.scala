package definiti.scalamodel.builder

import definiti.common.ast._
import definiti.scalamodel.ScalaAST

trait ClassDefinitionBuilder {
  self: ScalaModelBuilder =>

  def generateClassDefinition(classDefinition: ClassDefinition): Seq[ScalaAST.TopLevelElement] = classDefinition match {
    case definedType: DefinedType => generateDefinedType(definedType)
    case aliasType: AliasType => generateAliasType(aliasType)
    case enum: Enum => generateEnum(enum)
    case _ => throw new Exception(s"Unexpected type ${classDefinition}")
  }

  private def generateAliasType(aliasType: AliasType): Seq[ScalaAST.TopLevelElement] = {
    val commentElement = aliasType.comment.map(comment => ScalaAST.Comment(comment.trim))
    val atomicVerification = generatePublicAtomicVerification(aliasType)
    val dependentVerifications = generatePublicDependentVerifications(aliasType)
    val objectElement = ScalaAST.ObjectDef(
      name = aliasType.name,
      extendz = None,
      body = atomicVerification ++ dependentVerifications,
      property = None
    )

    commentElement ++ objectElement
  }

  private def generateDefinedType(definedType: DefinedType): Seq[ScalaAST.TopLevelElement] = {
    definedType.comment.map(comment => ScalaAST.Comment(comment.trim)) ++ Seq(
      generateCaseClass(definedType),
      generateCompanionObject(definedType)
    )
  }

  private def generateCaseClass(definedType: DefinedType): ScalaAST.TopLevelElement = {
    ScalaAST.CaseClassDef(
      name = definedType.name,
      extendz = None,
      parameters = definedType.attributes.map(attributeAsParameter),
      generics = definedType.genericTypes
    )
  }

  private def attributeAsParameter(attributeDefinition: AttributeDefinition): ScalaAST.Parameter = {
    ScalaAST.Parameter(
      name = attributeDefinition.name,
      typ = generateType(attributeDefinition.typeDeclaration)
    )
  }

  private def generateCompanionObject(definedType: DefinedType): ScalaAST.TopLevelElement = {
    val atomicVerification = generatePublicAtomicVerification(definedType)
    val dependentVerifications = generatePublicDependentVerifications(definedType)
    val jsonConverters = buildJsonConverter(definedType)
    ScalaAST.ObjectDef(
      name = definedType.name,
      extendz = None,
      body = atomicVerification ++ dependentVerifications ++ jsonConverters,
      property = None
    )
  }

  private def generateEnum(enum: Enum): Seq[ScalaAST.TopLevelElement] = {
    ScalaAST.ObjectDef(
      name = enum.name,
      extendz = ScalaAST.Extends(ScalaAST.Type("Enumeration"), Seq.empty),
      body = ScalaAST.SimpleExpression(s"val ${enum.cases.map(_.name).mkString(", ")} = Value"),
      property = None
    )
  }
}
