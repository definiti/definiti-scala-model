package definiti.scalamodel.builder

import definiti.core.ast._
import definiti.scalamodel.ScalaAST

trait NamedFunctionBuilder {
  self: ScalaModelBuilder =>

  def generateNamedFunction(namedFunction: NamedFunction): ScalaAST.Def1 = {
    ScalaAST.Def1(
      name = namedFunction.name,
      typ = namedFunction.returnType.typeName,
      generics = namedFunction.genericTypes,
      parameters = namedFunction.parameters.map { parameterDefinition =>
        ScalaAST.Parameter(
          name = parameterDefinition.name,
          typ = generateParameterType(parameterDefinition.typeReference),
          property = None
        )
      },
      body = Some(generateExpression(namedFunction.body))
    )
  }

}
