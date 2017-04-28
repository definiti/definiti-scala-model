package definiti.scalamodel

import definiti.scalamodel.ScalaAST.Expression

object ScalaCodeGenerator {

  def apply(ast: ScalaAST.Expression): String = generateExpression(ast)

  def withParens(ast: ScalaAST.Expression): String = ast match {
    case _: ScalaAST.Unambiguous => generateExpression(ast)
    case _ => s"(${generateExpression(ast)})"
  }

  def inBlock(ast: ScalaAST.Expression): String = inBlock(Seq(ast))
  def inBlock(ast: Seq[ScalaAST.Expression]): String = ast match {
    case (el: ScalaAST.Block) +: Seq() => generateExpression(el)
    case _ => generateExpression(ScalaAST.Block(ast))
  }

  val generateExpression: ScalaAST.Expression => String = {
    case ast: ScalaAST.SimpleExpression => generateSimpleExpression(ast)
    case ast: ScalaAST.BinaryOp => generateBinaryOp(ast)
    case ast: ScalaAST.UnaryOp => generateUnaryOp(ast)
    case ast: ScalaAST.Lambda => generateLambda(ast)
    case ast: ScalaAST.CallAttribute => generateCallAttribute(ast)
    case ast: ScalaAST.CallMethod => generateCallMethod(ast)
    case ast: ScalaAST.CallFunction => generateCallFunction(ast)
    case ast: ScalaAST.Block => generateBlock(ast)
    case ast: ScalaAST.If => generateIf(ast)
  }

  def generateSimpleExpression(ast: ScalaAST.SimpleExpression): String = ast.str

  def generateBinaryOp(ast: ScalaAST.BinaryOp): String =
    s"${withParens(ast.left)} ${ast.op} ${withParens(ast.right)}"

  def generateUnaryOp(ast: ScalaAST.UnaryOp): String =
    s"${ast.op} ${withParens(ast.inner)}"


  val generateIf: ScalaAST.If => String = {
    case ast: ScalaAST.IfThen => generateIfThen(ast)
    case ast: ScalaAST.IfThenElse => generateIfThenElse(ast)
  }

  def generateIfThen(ast: ScalaAST.IfThen): String =
    s"if (${generateExpression(ast.cond)}) ${inBlock(ast.ifTrue)}"

  def generateIfThenElse(ast: ScalaAST.IfThenElse): String =
    s"""if (${generateExpression(ast.cond)}) ${inBlock(ast.ifTrue)}
       |else ${inBlock(ast.ifTrue)}""".stripMargin

  def generateParameter(ast: ScalaAST.Parameter): String =
    s"${ast.name}: ${ast.typ}"

  def generateLambda(ast: ScalaAST.Lambda): String =
    s"(${ast.parameters.map(generateParameter).mkString(", ")}) => ${generateExpression(ast.body)}"

  def generateCallAttribute(ast: ScalaAST.CallAttribute): String =
    s"${withParens(ast.target)}.${ast.name}"

  def inParensOrBlock(ast: Seq[Expression]): String = ast match {
    case (el: ScalaAST.Block) +: Seq() => generateExpression(el)
    case _ => s"""(${ast.map(generateExpression).mkString(", ")})"""
  }

  def generateCallMethod(ast: ScalaAST.CallMethod): String =
    s"${withParens(ast.target)}.${ast.name}${inParensOrBlock(ast.arguments)}"

  def generateCallFunction(ast: ScalaAST.CallFunction): String =
    s"${generateExpression(ast.target)}${inParensOrBlock(ast.arguments)}"

  def generateBlock(ast: ScalaAST.Block): String = {
    s"""{
       |  ${ast.body.map(generateStatement).mkString("\n  ")}
       |}""".stripMargin
  }

  def generateComment(ast: ScalaAST.Comment): String = {
    s"/* ${ast.str} */"
  }

  def generateCommentedStatement(ast: ScalaAST.CommentedStatement): String = {
    s"""${generateComment(ast.comment)}
       |${generateStatement(ast.body)}""".stripMargin
  }

  def generateDef(ast: ScalaAST.Def): String =
    s"def ${ast.name}(${ast.parameters.map(generateParameter).mkString(", ")}): ${ast.typ} = ${inBlock(ast.body)}"

  val generateStatement: ScalaAST.Statement => String = {
    case ast: ScalaAST.Expression => generateExpression(ast)
    case ast: ScalaAST.CommentedStatement => generateCommentedStatement(ast)
    case ast: ScalaAST.Def => generateDef(ast)
  }

}
