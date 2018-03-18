package definiti.scalamodel.builder

import definiti.core.ast._
import definiti.scalamodel.ScalaAST

trait ClassDefinitionBuilder {
  self: ScalaModelBuilder =>

  def generateClassDefinition(classDefinition: ClassDefinition): Seq[ScalaAST.TopLevelElement] = classDefinition match {
    case definedType: DefinedType => generateDefinedType(definedType)
    case aliasType: AliasType => generateAliasType(aliasType)
    case enum: Enum => generateEnum(enum)
  }
}
