package definiti.scalamodel.generator

import definiti.scalamodel.ScalaAST
import definiti.scalamodel.ScalaAST.{Expression, Extends, Statement}

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
      case ast: ScalaAST.StringExpression => generateStringExpression(ast)
      case ast: ScalaAST.BinaryOp => generateBinaryOp(ast, indent)
      case ast: ScalaAST.UnaryOp => generateUnaryOp(ast, indent)
      case ast: ScalaAST.Lambda => generateLambda(ast, indent)
      case ast: ScalaAST.CallAttribute => generateCallAttribute(ast, indent)
      case ast: ScalaAST.CallMethod => generateCallMethod(ast, indent)
      case ast: ScalaAST.CallFunction => generateCallFunction(ast, indent)
      case ast: ScalaAST.Block => generateBlock(ast, indent)
      case ast: ScalaAST.If => generateIf(ast, indent)
      case ast: ScalaAST.New => generateNew(ast, indent)
    }
  }

  def generateSimpleExpression(ast: ScalaAST.SimpleExpression): String = ast.str

  def generateStringExpression(ast: ScalaAST.StringExpression): String = s""""${ast.string}""""

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

  def generateParameter(ast: ScalaAST.Parameter): String = {
    val property = ast.property.map(p => s"$p ").getOrElse("")
    val defaultValue = ast.defaultValue.map(defaultValue => s" = ${generateExpression(defaultValue, "")}").getOrElse("")
    s"${property}${ast.name}: ${ast.typ}${defaultValue}"
  }

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
       |${inc(indent)}${ast.body.map(a => generateStatement(a, inc(indent))).mkString(s"\n${inc(indent)}")}
       |$indent}""".stripMargin

  def generateNew(ast: ScalaAST.New, indent: String): String = {
    val generics = if (ast.generics.nonEmpty) ast.generics.mkString("[", ", ", "]") else ""
    val parameters = ast.arguments.map(a => generateExpression(a, indent)).mkString(", ")
    s"new ${ast.name}${generics}(${parameters})"
  }

  def generateComment(ast: ScalaAST.Comment, indent: String): String =
    if ((indent.length + ast.str.length < 120) && !ast.str.contains("\n")) s"// ${ast.str}"
    else s"/* ${ast.str.replace("\n", s"\n$indent")} */"

  def generateDef0(ast: ScalaAST.Def0, indent: String): String =
    s"""${ast.property.map(p => s"$p ").getOrElse("")}def ${ast.name}${generateGenerics(ast.generics)}: ${ast.typ}${ast.body.map(a => " = " + maybeInBlock(a, indent)).getOrElse("")}"""

  def generateDef1(ast: ScalaAST.Def1, indent: String): String =
    s"""${ast.property.map(p => s"$p ").getOrElse("")}def ${ast.name}${generateGenerics(ast.generics)}(${ast.parameters.map(generateParameter).mkString(", ")}): ${ast.typ}${ast.body.map(a => " = " + maybeInBlock(a, indent)).getOrElse("")}"""

  def generateDef2(ast: ScalaAST.Def2, indent: String): String =
    s"""${ast.property.map(p => s"$p ").getOrElse("")}def ${ast.name}${generateGenerics(ast.generics)}(${ast.parameters1.map(generateParameter).mkString(", ")})(${ast.parameters2.map(generateParameter).mkString(", ")}): ${ast.typ}${ast.body.map(a => " = " + maybeInBlock(a, indent)).getOrElse("")}"""

  private def generateGenerics(generics: Seq[String]): String = {
    if (generics.nonEmpty) {
      generics.mkString("[", ",", "]")
    } else {
      ""
    }
  }

  def generatePackageDeclaration(ast: ScalaAST.PackageDeclaration): String =
    s"package ${ast.name}"

  def generateImport(ast: ScalaAST.Import): String =
    s"import ${ast.name}"

  def generatePackageDef(ast: ScalaAST.PackageDef, indent: String): String =
    s"""package object ${ast.name} {
       |${inc(indent)}${ast.body.map(a => generateStatement(a, inc(indent))).mkString(s"\n${inc(indent)}")}
       |$indent}""".stripMargin

  def generateStatementsGroup(ast: ScalaAST.StatementsGroup, indent: String): String =
    ast.statements.map(a => generateStatement(a, indent)).mkString(s"\n$indent")

  def generateVal(ast: ScalaAST.Val, indent: String): String = {
    val properties = Seq(
      if (ast.isImplicit) "implicit" else "",
      if (ast.isLazy) "lazy" else ""
    ).filter(_.nonEmpty).mkString(" ")
    s"$properties val ${ast.name} = ${generateExpression(ast.value, indent)}"
  }

  def generateTraitDef(ast: ScalaAST.TraitDef, indent: String): String = {
    val declaration = if (ast.isSealed) s"sealed trait ${ast.name}" else s"trait ${ast.name}"
    val content = if (ast.body.nonEmpty) {
      s"""{
         |${inc(indent)}${ast.body.map(a => generateStatement(a, inc(indent))).mkString(s"\n${inc(indent)}")}
         |$indent}""".stripMargin
    } else {
      ""
    }
    declaration + content
  }

  def generateClassDef(ast: ScalaAST.ClassDef, indent: String): String = {
    val property = ast.property.map(p => s"$p ").getOrElse("")
    val privateProperty = if (ast.privateConstructor) " private" else ""
    val generics = if (ast.generics.nonEmpty) ast.generics.mkString("[", ", ", "]") else ""
    val parameters = ast.parameters.map(generateParameter).mkString(", ")
    val extendz = ast.extendz.map(generateExtends).getOrElse("")
    val body = if (ast.body.isEmpty) "" else " " + generateStatement(ScalaAST.Block(ast.body), indent)
    s"""${property}class ${ast.name}${privateProperty}${generics}(${parameters})${extendz}${body}"""
  }

  def generateExtends(extendz: Extends): String = {
    if (extendz.parameters.nonEmpty) {
      s" extends ${extendz.typ.toCode}(${extendz.parameters.map(generateStatement(_, "")).mkString(", ")})"
    } else {
      s" extends ${extendz.typ.toCode}"
    }
  }

  def generateCaseClassDef(ast: ScalaAST.CaseClassDef, indent: String): String = {
    s"""${ast.property.map(p => s"$p ").getOrElse("")}case class ${ast.name}${generateGenerics(ast.generics)}(${ast.parameters.map(generateParameter).mkString(", ")})${ast.extendz.map(e => s" extends $e").getOrElse("")}${if (ast.body.isEmpty) "" else " " + generateStatement(ScalaAST.Block(ast.body), indent)}"""
  }

  def generateCase(ast: ScalaAST.Case, indent: String): String =
    s"case ${ast.pattern} => ${generateStatement(ast.body, indent)}"

  def generateMatch(ast: ScalaAST.Match, indent: String): String =
    s"""${generateExpression(ast.expr, indent)} match {
       |${inc(indent)}${ast.cases.map(a => generateCase(a, inc(indent))).mkString(s"\n${inc(indent)}")}
       |$indent}""".stripMargin

  def generateObjectDef(ast: ScalaAST.ObjectDef, indent: String): String =
    s"""object ${ast.name}${ast.extendz.map(generateExtends).getOrElse("")} {
       |${inc(indent)}${ast.body.map(a => generateStatement(a, inc(indent))).mkString(s"\n${inc(indent)}")}
       |$indent}""".stripMargin

  def generateClassVal(ast: ScalaAST.ClassVal, indent: String): String = {
    val properties = Seq(
      if (ast.isPrivate) "private" else "",
      if (ast.isImplicit) "implicit" else "",
      if (ast.isLazy) "lazy" else ""
    ).filter(_.nonEmpty).mkString(" ")
    val declaration = s"$properties val ${ast.name}: ${ast.typ}".trim
    val assignation = generateStatements(ast.body, indent)
    s"$declaration = $assignation"
  }

  def generateTypeDef(ast: ScalaAST.TypeDef, indent: String): String = {
    s"type ${ast.name} = ${ast.typ}"
  }

  def generateStatements(statements: Seq[Statement], indent: String): String = {
    val innerIndent = inc(indent)
    if (statements.isEmpty) {
      "{}"
    } else if (statements.lengthCompare(1) == 0) {
      generateStatement(statements.head, innerIndent)
    } else {
      statements
        .map(a => generateStatement(a, innerIndent))
        .mkString(s"{\n${innerIndent}", s"\n${innerIndent}", s"\n${indent}}")
    }
  }

  def generateStatement(ast: ScalaAST.Statement, indent: String): String = {
    ast match {
      case ast: ScalaAST.Expression => generateExpression(ast, indent)
      case ast: ScalaAST.Comment => generateComment(ast, indent)
      case ast: ScalaAST.Def0 => generateDef0(ast, indent)
      case ast: ScalaAST.Def1 => generateDef1(ast, indent)
      case ast: ScalaAST.Def2 => generateDef2(ast, indent)
      case ast: ScalaAST.PackageDeclaration => generatePackageDeclaration(ast)
      case ast: ScalaAST.Import => generateImport(ast)
      case ScalaAST.Blank => ""
      case ast: ScalaAST.PackageDef => generatePackageDef(ast, indent)
      case ast: ScalaAST.StatementsGroup => generateStatementsGroup(ast, indent)
      case ast: ScalaAST.Val => generateVal(ast, indent)
      case ast: ScalaAST.TraitDef => generateTraitDef(ast, indent)
      case ast: ScalaAST.ClassDef => generateClassDef(ast, indent)
      case ast: ScalaAST.CaseClassDef => generateCaseClassDef(ast, indent)
      case ast: ScalaAST.Match => generateMatch(ast, indent)
      case ast: ScalaAST.ObjectDef => generateObjectDef(ast, indent)
      case ast: ScalaAST.ClassVal => generateClassVal(ast, indent)
      case ast: ScalaAST.TypeDef => generateTypeDef(ast, indent)
    }
  }

}
