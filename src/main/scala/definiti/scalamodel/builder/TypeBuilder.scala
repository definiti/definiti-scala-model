package definiti.scalamodel.builder

import definiti.core.ast._
import definiti.scalamodel.ScalaAST

trait TypeBuilder {
  self: ScalaModelBuilder =>

  def generateParameterType(typeReference: AbstractTypeReference): String = {
    typeReference match {
      case typeReference: TypeReference =>
        val finalTypeName = generateMainType(typeReference)
        val parameterGenerics = generateGenericTypes(typeReference.genericTypes)
        finalTypeName + parameterGenerics

      case LambdaReference(inputTypes, outputType) =>
        def generateOneType(typeReference: TypeReference): String = {
          val typeName = typeReference.typeName
          val generics = generateGenericTypes(typeReference.genericTypes)
          typeName + generics
        }

        s"(${inputTypes.map(generateOneType)}) => ${generateOneType(outputType)}"
    }
  }

  def generateMainType(typeReference: TypeReference): String = {
    library.types.get(typeReference.typeName) match {
      case Some(_: NativeClassDefinition) => nativeTypeMapping.getOrElse(typeReference.typeName, typeReference.typeName)
      case Some(aliasType: AliasType) => generateMainType(aliasType.alias)
      case _ => typeReference.typeName
    }
  }

  def generateGenericTypes(genericTypes: Seq[TypeReference]): String = {
    def generateGenericType(genericType: TypeReference): String = {
      nativeTypeMapping.getOrElse(genericType.typeName, genericType.typeName) + generateGenericTypes(genericType.genericTypes)
    }

    if (genericTypes.nonEmpty) {
      genericTypes.map(generateGenericType).mkString("[", ",", "]")
    } else {
      ""
    }
  }

  def generateTag(aliasType: AliasType): ScalaAST.Statement = {
    ScalaAST.StatementsGroup(
      ScalaAST.TraitDef(s"${aliasType.name}Tag", Seq.empty, isSealed = true),
      ScalaAST.TypeDef(aliasType.name, s"${generateParameterType(aliasType.alias)} @@ ${aliasType.name}Tag")
    )
  }

  def generateGenericTypeDefinition(definedType: DefinedType): String = {
    if (definedType.genericTypes.nonEmpty) {
      definedType.genericTypes.mkString("[", ",", "]")
    } else {
      ""
    }
  }

  def verificationsFromTypeReference(typeReference: TypeReference): Seq[VerificationReference] = {
    library.types.get(typeReference.typeName)
      .toSeq
      .flatMap(verificationsFromType)
  }

  def verificationsFromType(classDefinition: ClassDefinition): Seq[VerificationReference] = {
    classDefinition match {
      case aliasType: AliasType => verificationsFromTypeReference(aliasType.alias) ++ aliasType.inherited
      case definedType: DefinedType => definedType.inherited
      case _ => Seq.empty
    }
  }

  def internalVerificationsFromTypeReference(typeReference: TypeReference): Seq[TypeVerification] = {
    library.types.get(typeReference.typeName)
      .toSeq
      .flatMap(internalVerificationsFromType)
  }

  def internalVerificationsFromType(classDefinition: ClassDefinition): Seq[TypeVerification] = {
    classDefinition match {
      case aliasType: AliasType => internalVerificationsFromTypeReference(aliasType.alias)
      case definedType: DefinedType => definedType.verifications
      case _ => Seq.empty
    }
  }
}
