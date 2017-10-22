package definiti.scalamodel.builder

import definiti.core.ast._
import definiti.scalamodel.ScalaAST

trait TypeBuilder {
  self: ScalaModelBuilder =>

  def generateParameterType(typeReference: AbstractTypeReference): String = {
    typeReference match {
      case TypeReference(typeName, genericTypes) =>
        val finalTypeName = library.types.get(typeName) match {
          case Some(_: NativeClassDefinition) => nativeTypeMapping.getOrElse(typeName, typeName)
          case Some(aliasType: AliasType) => generateParameterType(aliasType.alias)
          case _ => typeName
        }
        val parameterGenerics = generateGenericTypes(genericTypes)
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
}
