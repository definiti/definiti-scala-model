package definiti.scalamodel

import definiti.scalamodel.ScalaAST.Expression

object ScalaCodeGenerator {

  def apply(ast: ScalaAST.Statement): String = generateStatement(ast, "")
  def inc(indent: String) = s"$indent  "

  def inParens(ast: ScalaAST.Expression, indent: String): String = ast match {
    case _: ScalaAST.Unambiguous => generateExpression(ast, indent)
    case _ => s"(${generateExpression(ast, indent)})"
  }
  def inParensOrBlock(ast: Seq[Expression], indent: String): String = ast match {
    case (el: ScalaAST.Block) +: Seq() => generateExpression(el, indent)
    case _ => s"""(${ast.map(a => generateExpression(a, indent)).mkString(", ")})"""
  }

  def maybeInBlock(ast: ScalaAST.Expression, indent: String): String = maybeInBlock(Seq(ast), indent)
  def maybeInBlock(ast: Seq[ScalaAST.Expression], indent: String): String = ast match {
    case (el: ScalaAST.Unambiguous) +: Seq() => generateExpression(el, indent)
    case (el: ScalaAST.Block) +: Seq() => generateExpression(el, indent)
    case _ => generateExpression(ScalaAST.Block(ast), indent)
  }

  def inBlock(ast: ScalaAST.Expression, indent: String): String = inBlock(Seq(ast), indent)
  def inBlock(ast: Seq[ScalaAST.Expression], indent: String): String = ast match {
    case (el: ScalaAST.Block) +: Seq() => generateExpression(el, indent)
    case _ => generateExpression(ScalaAST.Block(ast), indent)
  }

  def generateExpression(ast: ScalaAST.Expression, indent: String): String = {
    ast match {
      case ast: ScalaAST.SimpleExpression => generateSimpleExpression(ast)
      case ast: ScalaAST.BinaryOp => generateBinaryOp(ast, indent)
      case ast: ScalaAST.UnaryOp => generateUnaryOp(ast, indent)
      case ast: ScalaAST.Lambda => generateLambda(ast, indent)
      case ast: ScalaAST.CallAttribute => generateCallAttribute(ast, indent)
      case ast: ScalaAST.CallMethod => generateCallMethod(ast, indent)
      case ast: ScalaAST.CallFunction => generateCallFunction(ast, indent)
      case ast: ScalaAST.Block => generateBlock(ast, indent)
      case ast: ScalaAST.If => generateIf(ast, indent)
    }
  }

  def generateSimpleExpression(ast: ScalaAST.SimpleExpression): String = ast.str

  def generateBinaryOp(ast: ScalaAST.BinaryOp, indent: String): String =
    s"${inParens(ast.left, indent)} ${ast.op} ${inParens(ast.right, indent)}"

  def generateUnaryOp(ast: ScalaAST.UnaryOp, indent: String): String =
    s"${ast.op} ${inParens(ast.inner, indent)}"


  def generateIf(ast: ScalaAST.If, indent: String): String = {
    ast match {
      case ast: ScalaAST.IfThen => generateIfThen(ast, indent)
      case ast: ScalaAST.IfThenElse => generateIfThenElse(ast, indent)
    }
  }

  def generateIfThen(ast: ScalaAST.IfThen, indent: String): String =
    s"if (${generateExpression(ast.cond, indent)}) ${maybeInBlock(ast.ifTrue, indent)}"

  def generateIfThenElse(ast: ScalaAST.IfThenElse, indent: String): String =
    s"""if (${generateExpression(ast.cond, indent)}) ${maybeInBlock(ast.ifTrue, indent)}
       |${indent}else ${maybeInBlock(ast.ifFalse, indent)}""".stripMargin

  def generateParameter(ast: ScalaAST.Parameter): String =
    s"${ast.name}: ${ast.typ}"

  def generateLambda(ast: ScalaAST.Lambda, indent: String): String =
    s"(${ast.parameters.map(generateParameter).mkString(", ")}) => ${generateExpression(ast.body, indent)}"

  def generateCallAttribute(ast: ScalaAST.CallAttribute, indent: String): String =
    s"${inParens(ast.target, indent)}.${ast.name}"

  def generateCallMethod(ast: ScalaAST.CallMethod, indent: String): String =
    s"${inParens(ast.target, indent)}.${ast.name}${inParensOrBlock(ast.arguments, indent)}"

  def generateCallFunction(ast: ScalaAST.CallFunction, indent: String): String =
    s"${generateExpression(ast.target, indent)}${inParensOrBlock(ast.arguments, indent)}"

  def generateBlock(ast: ScalaAST.Block, indent: String): String =
    s"""{
       |$indent  ${ast.body.map(a => generateStatement(a, inc(indent))).mkString("\n")}
       |$indent}""".stripMargin

  def generateComment(ast: ScalaAST.Comment, indent: String): String =
    if ((indent.length + ast.str.length < 120) && !ast.str.contains("\n")) s"// ${ast.str}"
    else s"/* ${ast.str.replace("\n", s"\n$indent")} */"

  def generateDef(ast: ScalaAST.Def, indent: String): String =
    s"${ast.property.map(p => s"$p ").getOrElse("")}def ${ast.name}(${ast.parameters.map(generateParameter).mkString(", ")}): ${ast.typ} = ${maybeInBlock(ast.body, indent)}"

  def generateDef2(ast: ScalaAST.Def2, indent: String): String =
    s"${ast.property.map(p => s"$p ").getOrElse("")}def ${ast.name}(${ast.parameters1.map(generateParameter).mkString(", ")})(${ast.parameters2.map(generateParameter).mkString(", ")}): ${ast.typ} = ${maybeInBlock(ast.body, indent)}"

  def generateImport(ast: ScalaAST.Import): String =
    s"import ${ast.name}"

  def generatePackageDef(ast: ScalaAST.PackageDef, indent: String): String =
    s"""package ${ast.name} {
       |${inc(indent)}${ast.body.map(a => generateStatement(a, inc(indent))).mkString(s"\n${inc(indent)}")}
       |$indent}""".stripMargin

  def generateStatementsGroup(ast: ScalaAST.StatementsGroup, indent: String): String =
    ast.statements.map(a => generateStatement(a, indent)).mkString(s"\n$indent")

  def generateStatement(ast: ScalaAST.Statement, indent: String): String = {
    ast match {
      case ast: ScalaAST.Expression => generateExpression(ast, indent)
      case ast: ScalaAST.Comment => generateComment(ast, indent)
      case ast: ScalaAST.Def => generateDef(ast, indent)
      case ast: ScalaAST.Def2 => generateDef2(ast, indent)
      case ast: ScalaAST.Import => generateImport(ast)
      case ast: ScalaAST.PackageDef => generatePackageDef(ast, indent)
      case ast: ScalaAST.StatementsGroup => generateStatementsGroup(ast, indent)
    }
  }

}
