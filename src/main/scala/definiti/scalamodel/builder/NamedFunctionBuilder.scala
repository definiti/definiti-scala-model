package definiti.scalamodel.builder

import definiti.core.ast._
import definiti.scalamodel.ScalaAST

trait NamedFunctionBuilder {
  self: ScalaModelBuilder =>

  def generateNamedFunction(namedFunction: NamedFunction): Seq[ScalaAST.TopLevelElement] = {
    Seq(
      ScalaAST.Def1(
        name = namedFunction.name,
        typ = generateType(namedFunction.returnType),
        generics = namedFunction.genericTypes,
        parameters = namedFunction.parameters.map { parameterDefinition =>
          ScalaAST.Parameter(
            name = parameterDefinition.name,
            typ = generateType(parameterDefinition.typeReference),
            property = None
          )
        },
        body = Some(generateExpression(namedFunction.body))
      )
    )
  }

}
