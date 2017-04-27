package definiti.scalamodel

object ScalaCodeGenerator {

  def apply(ast: ScalaAST.Expression): String = generateExpression(ast)

  def withParens(ast: ScalaAST.Expression): String = ast match {
    case _: ScalaAST.Unambiguous => generateExpression(ast)
    case _ => s"(${generateExpression(ast)})"
  }

  val generateExpression: ScalaAST.Expression => String = {
    case ast: ScalaAST.SimpleExpression => generateSimpleExpression(ast)
    case ast: ScalaAST.BinaryOp => generateBinaryOp(ast)
    case ast: ScalaAST.UnaryOp => generateUnaryOp(ast)
    case ast: ScalaAST.Lambda => generateLambda(ast)
    case ast: ScalaAST.CallAttribute => generateCallAttribute(ast)
    case ast: ScalaAST.CallMethod => generateCallMethod(ast)
    case ast: ScalaAST.CallFunction2 => generateCallFunction2(ast)
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
    s"if (${generateExpression(ast.cond)}) ${generateExpression(ast.ifTrue)}"

  def generateIfThenElse(ast: ScalaAST.IfThenElse): String =
    s"""if (${generateExpression(ast.cond)}) {
       |  ${generateExpression(ast.ifTrue)}
       |} else {
       |  ${generateExpression(ast.ifTrue)}
       |}""".stripMargin

  def generateParameter(ast: ScalaAST.Parameter): String =
    s"${ast.name}: ${ast.typ}"

  def generateLambda(ast: ScalaAST.Lambda): String =
    s"(${ast.parameters.map(generateParameter).mkString(", ")}) => ${generateExpression(ast.body)}"

  def generateCallAttribute(ast: ScalaAST.CallAttribute): String =
    s"${withParens(ast.target)}.${ast.name}"

  def generateCallMethod(ast: ScalaAST.CallMethod): String =
    s"${withParens(ast.target)}.${ast.name}(${ast.arguments.map(generateExpression).mkString(", ")})"

  def generateCallFunction2(ast: ScalaAST.CallFunction2): String =
    s"${ast.name}(${ast.arguments1.map(generateExpression).mkString(", ")})(${ast.arguments2.map(generateExpression).mkString(", ")})"

  def generateBlock(ast: ScalaAST.Block): String = {
    s"{\n${ast.body.map(generateStatement).mkString("\n")}\n}"
  }

  def generateComment(ast: ScalaAST.Comment): String = {
    s"/* ${ast.str} */"
  }

  def generateCommentedStatement(ast: ScalaAST.CommentedStatement): String = {
    s"""${generateComment(ast.comment)}
       |${generateStatement(ast.body)}""".stripMargin
  }

  def generateDef(ast: ScalaAST.Def): String =
    s"""def ${ast.name}(${ast.parameters.map(generateParameter).mkString(", ")}): ${ast.typ} = {
       |  ${generateExpression(ast.body)}
       |}""".stripMargin

  val generateStatement: ScalaAST.Statement => String = {
    case ast: ScalaAST.Expression => generateExpression(ast)
    case ast: ScalaAST.CommentedStatement => generateCommentedStatement(ast)
    case ast: ScalaAST.Def => generateDef(ast)
  }

}
