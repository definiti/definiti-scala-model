package definiti.scalamodel.builder

import definiti.core.ast._
import definiti.scalamodel.ScalaAST
import definiti.scalamodel.utils.{ListUtils, StringUtils}

trait TypeBuilder {
  self: ScalaModelBuilder =>

  def generateType(typeReference: AbstractTypeReference): String = {
    typeReference match {
      case typeReference: TypeReference =>
        generateScalaType(typeReference).toCode

      case LambdaReference(inputTypes, outputType) =>
        def generateOneType(typeReference: TypeReference): String = {
          val typeName = StringUtils.lastPart(typeReference.typeName)
          val generics = generateGenericTypes(typeReference.genericTypes)
          typeName + generics
        }

        s"(${inputTypes.map(generateOneType)}) => ${generateOneType(outputType)}"
    }
  }

  def generateScalaType(typeReference: TypeReference): ScalaAST.Type = {
    def process(typeReference: TypeReference, outerTypes: Seq[TypeReference]): ScalaAST.Type = {
      library.types.get(typeReference.typeName) match {
        case Some(_: NativeClassDefinition) =>
          ScalaAST.Type(
            name = nativeTypeMapping.getOrElse(typeReference.typeName, typeReference.typeName),
            generics = ListUtils.replaceOrdered(typeReference.genericTypes, outerTypes).map(process(_, Seq.empty))
          )
        case Some(aliasType: AliasType) =>
          process(aliasType.alias, ListUtils.replaceOrdered(aliasType.alias.genericTypes, typeReference.genericTypes))
        case _ =>
          ScalaAST.Type(
            name = StringUtils.lastPart(typeReference.typeName),
            generics = ListUtils.replaceOrdered(typeReference.genericTypes, outerTypes).map(process(_, Seq.empty))
          )
      }
    }
    process(typeReference, Seq.empty)
  }

  def generateGenericsOfTypes(typeReference: TypeReference): String = {
    generateGenericsOfTypes(typeReference, typeReference.genericTypes)
  }
  def generateGenericsOfTypes(typeReference: TypeReference, outerTypeReferences: Seq[TypeReference]): String = {
    library.types.get(typeReference.typeName) match {
      case Some(_: NativeClassDefinition) =>
        generateGenericTypes(outerTypeReferences)
      case Some(aliasType: AliasType) =>
        generateGenericsOfTypes(aliasType.alias, ListUtils.replaceOrdered(aliasType.alias.genericTypes, outerTypeReferences))
      case _ =>
        generateGenericTypes(outerTypeReferences)
    }
  }

  def generateGenericTypes(genericTypes: Seq[TypeReference]): String = {
    def generateGenericType(genericType: TypeReference): String = {
      val mainType = library.types.get(genericType.typeName) match {
        case Some(aliasType: AliasType) => generateGenericType(aliasType.alias)
        case _ => nativeTypeMapping.getOrElse(genericType.typeName, StringUtils.lastPart(genericType.typeName))
      }
      mainType + generateGenericTypes(genericType.genericTypes)
    }

    if (genericTypes.nonEmpty) {
      genericTypes.map(generateGenericType).mkString("[", ",", "]")
    } else {
      ""
    }
  }

  def verificationsFromTypeReference(typeReference: TypeReference): Seq[VerificationReference] = {
    library.types.get(typeReference.typeName)
      .collect { case definedType: DefinedType => definedType }
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

  def generateGenerics(genericTypes: Seq[String]): String = {
    if (genericTypes.nonEmpty) {
      genericTypes.mkString("[", ",", "]")
    } else {
      ""
    }
  }

  def isNative(typeReference: TypeReference): Boolean = {
    library.types.get(typeReference.typeName).exists {
      case _: NativeClassDefinition => true
      case alias: AliasType => isNative(alias.alias)
      case _ => false
    }
  }

  def isNative(typ: ScalaAST.Type): Boolean = isNative(typ.name)

  def isNative(typ: String): Boolean = {
    nativeTypeMapping.values.exists(_ == typ)
  }

  def isList(typeReference: TypeReference): Boolean = {
    library.types.get(typeReference.typeName).exists {
      case native: NativeClassDefinition => native.name == "List"
      case alias: AliasType => isList(alias.alias)
      case _ => false
    }
  }

  def isOption(typeReference: TypeReference): Boolean = {
    library.types.get(typeReference.typeName).exists {
      case native: NativeClassDefinition => native.name == "Option"
      case alias: AliasType => isList(alias.alias)
      case _ => false
    }
  }
}
