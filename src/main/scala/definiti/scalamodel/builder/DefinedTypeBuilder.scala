package definiti.scalamodel.builder

import definiti.core.ast._
import definiti.scalamodel.ScalaAST
import definiti.scalamodel.utils.{ListUtils, StringUtils}

trait DefinedTypeBuilder {
  self: ScalaModelBuilder =>

  def generateDefinedType(definedType: DefinedType): Seq[ScalaAST.TopLevelElement] = {
    Seq(
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

  def attributeAsParameter(attributeDefinition: AttributeDefinition): ScalaAST.Parameter = {
    ScalaAST.Parameter(
      name = attributeDefinition.name,
      typ = generateType(attributeDefinition.typeDeclaration).toCode
    )
  }

  def generateCompanionObject(definedType: DefinedType): ScalaAST.TopLevelElement = {
    val atomicVerification = generatePublicAtomicVerification(definedType)
    val dependentVerifications = generatePublicDependentVerifications(definedType)
    val jsonConverters = buildJsonConverter(definedType)
    ScalaAST.ObjectDef(
      name = definedType.name,
      extendz = None,
      body = (atomicVerification +: dependentVerifications) ++ jsonConverters,
      property = None
    )
  }
}
