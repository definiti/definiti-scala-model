package definiti.scalamodel.builder.typeVerification

import definiti.common.ast._
import definiti.scalamodel.ScalaAST
import definiti.scalamodel.builder.ScalaModelBuilder
import definiti.scalamodel.utils.{ListUtils, Memoizer}

trait TypeVerificationInformation {
  self: ScalaModelBuilder =>

  private val extractDependentTypeVerificationNamesMemoizer = new Memoizer[Seq[String]]

  def extractDependentTypeVerificationNames(classDefinition: ClassDefinition): Seq[String] = {
    extractDependentTypeVerificationNamesMemoizer.value(classDefinition.fullName) {
      classDefinition match {
        case definedType: DefinedType =>
          val attributeVerificationNames = definedType.attributes
            .map(_.typeDeclaration)
            .flatMap(extractTypeNames)
            .distinct
            .flatMap(library.typesMap.get)
            .flatMap(extractDependentTypeVerificationNames)

          val directVerificationNames = definedType.verifications.collect {
            case typeVerification: DependentTypeVerification => typeVerification.name
          }
          (attributeVerificationNames ++ directVerificationNames).distinct
        case aliasType: AliasType =>
          val aliasVerificationNames = library.typesMap
            .get(aliasType.alias.typeName)
            .map(extractDependentTypeVerificationNames)
            .getOrElse(Seq.empty)
          val directVerificationNames = aliasType.verifications.collect {
            case typeVerification: DependentTypeVerification => typeVerification.name
          }
          (aliasVerificationNames ++ directVerificationNames).distinct
        case _ => Seq.empty
      }
    }
  }

  private def extractTypeNames(typeDeclaration: TypeDeclaration): Seq[String] = {
    typeDeclaration.typeName +: typeDeclaration.genericTypes.flatMap(extractTypeNames)
  }

  private val hasDependentTypeVerificationMemoizer = new Memoizer[Boolean]

  def hasDependentTypeVerification(classDefinition: ClassDefinition, name: String): Boolean = {
    hasDependentTypeVerificationMemoizer.value(s"${classDefinition.fullName}.${name}") {
      extractDependentTypeVerificationNames(classDefinition).contains(name)
    }
  }

  private val extractDependentTypeParametersMemoizer = new Memoizer[Seq[ScalaAST.Parameter]]

  def extractDependentTypeParameters(classDefinition: ClassDefinition, name: String): Seq[ScalaAST.Parameter] = {
    extractDependentTypeParametersMemoizer.value(s"${classDefinition.fullName}.${name}") {
      classDefinition match {
        case definedType: DefinedType =>
          val attributeVerificationNames = definedType.attributes
            .map(_.typeDeclaration)
            .flatMap(extractTypeNames)
            .distinct
            .flatMap(library.typesMap.get)
            .flatMap(extractDependentTypeParameters(_, name))

          val directVerificationNames = definedType.verifications
            .collect { case typeVerification: DependentTypeVerification if typeVerification.name == name =>
                typeVerification.function.parameters.tail.map(generateParameter)
            }
            .flatten
          ListUtils.distinctBy(attributeVerificationNames ++ directVerificationNames, (x: ScalaAST.Parameter) => x.name)
        case aliasType: AliasType =>
          val aliasVerificationNames = library.typesMap
            .get(aliasType.alias.typeName)
            .map(extractDependentTypeParameters(_, name))
            .getOrElse(Seq.empty)
          val directVerificationNames = aliasType.verifications
            .collect { case typeVerification: DependentTypeVerification if typeVerification.name == name =>
              typeVerification.function.parameters.tail.map(generateParameter)
            }
            .flatten
          ListUtils.distinctBy(aliasVerificationNames ++ directVerificationNames, (x: ScalaAST.Parameter) => x.name)
        case _ => Seq.empty
      }
    }
  }
}
