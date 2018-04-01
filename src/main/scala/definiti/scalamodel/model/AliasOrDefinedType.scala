package definiti.scalamodel.model

import definiti.core.ast._

case class AliasOrDefinedType(
  name: String,
  fullName: String,
  genericTypes: Seq[String],
  parameters: Seq[ParameterDefinition],
  verifications: Seq[TypeVerification],
  inherited: Seq[VerificationReference],
  comment: Option[String],
  location: Location,
  internal: ProjectClassDefinition
)

object AliasOrDefinedType {
  implicit def aliasTypeToAliasOrDefinedType(aliasType: AliasType): AliasOrDefinedType = {
    AliasOrDefinedType(
      name = aliasType.name,
      fullName = aliasType.fullName,
      genericTypes = aliasType.genericTypes,
      parameters = aliasType.parameters,
      verifications = aliasType.verifications,
      inherited = aliasType.inherited,
      comment = aliasType.comment,
      location = aliasType.location,
      internal = aliasType
    )
  }

  implicit def aliasTypeToAliasOrDefinedType(definedType: DefinedType): AliasOrDefinedType = {
    AliasOrDefinedType(
      name = definedType.name,
      fullName = definedType.fullName,
      genericTypes = definedType.genericTypes,
      parameters = definedType.parameters,
      verifications = definedType.verifications,
      inherited = definedType.inherited,
      comment = definedType.comment,
      location = definedType.location,
      internal = definedType
    )
  }

  implicit def aliasOrDefinedTypeToProjectClassDefinition(aliasOrDefinedType: AliasOrDefinedType): ProjectClassDefinition = {
    aliasOrDefinedType.internal
  }
}