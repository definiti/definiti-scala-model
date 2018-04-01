package definiti.scalamodel.builder

import definiti.core.ast._
import definiti.scalamodel.ScalaAST
import definiti.scalamodel.utils.{ListUtils, StringUtils}

trait AliasTypeBuilder {
  self: ScalaModelBuilder =>

  def generateAliasType(aliasType: AliasType): Seq[ScalaAST.TopLevelElement] = {
    val commentElement = aliasType.comment.map(comment => ScalaAST.Comment(comment.trim))
    val atomicVerification = generatePublicAtomicVerification(aliasType)
    val dependentVerifications = generatePublicDependentVerifications(aliasType)
    val objectElement = ScalaAST.ObjectDef(
      name = aliasType.name,
      extendz = None,
      body = atomicVerification +: dependentVerifications,
      property = None
    )

    commentElement.toSeq :+ objectElement
  }
}
