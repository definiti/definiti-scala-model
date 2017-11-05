package definiti.scalamodel

import java.nio.file.Path

object ScalaAST {

  sealed trait Statement

  sealed trait Expression extends Statement

  sealed trait Unambiguous // no parentheses required

  case class SimpleExpression(str: String) extends Expression with Unambiguous
  case class StringExpression(string: String) extends Expression with Unambiguous

  case class BinaryOp(op: String, left: Expression, right: Expression) extends Expression
  case class UnaryOp(op: String, inner: Expression) extends Expression

  sealed trait If extends Expression
  case class IfThen(cond: Expression, ifTrue: Expression) extends If
  case class IfThenElse(cond: Expression, ifTrue: Expression, ifFalse: Expression) extends If

  case class Parameter(name: String, typ: String, defaultValue: Option[Expression] = None, property: Option[String] = None)
  case class Lambda(parameters: Seq[Parameter], body: Expression) extends Expression

  case class CallAttribute(target: Expression, name: String) extends Expression with Unambiguous
  case class CallMethod(target: Expression, name: String, arguments: Seq[Expression]) extends Expression with Unambiguous

  object CallMethod {
    def apply(target: String, name: String, arguments: Expression*): CallMethod = {
      new CallMethod(ScalaAST.SimpleExpression(target), name, arguments)
    }
  }

  case class CallFunction(target: Expression, arguments: Seq[Expression]) extends Expression with Unambiguous

  object CallFunction {
    def apply(target: String, arguments: Expression*): CallFunction = {
      new CallFunction(SimpleExpression(target), arguments)
    }
  }

  case class Block(body: Seq[Statement]) extends Expression

  object Block {
    def apply(body: Statement*)(implicit dummyImplicit: DummyImplicit): Block = new Block(body)
  }

  case class New(name: String, arguments: Seq[Expression]) extends Expression

  case class Comment(str: String) extends Statement

  case class Def0(
    name: String,
    typ: String,
    generics: Seq[String] = Seq.empty,
    body: Option[Expression] = None,
    property: Option[String] = None
  ) extends Statement

  case class Def1(
    name: String,
    typ: String,
    generics: Seq[String] = Seq.empty,
    parameters: Seq[Parameter] = Seq.empty,
    body: Option[Expression] = None,
    property: Option[String] = None
  ) extends Statement

  case class Def2(
    name: String,
    typ: String,
    generics: Seq[String] = Seq.empty,
    parameters1: Seq[Parameter] = Seq.empty,
    parameters2: Seq[Parameter] = Seq.empty,
    body: Option[Expression] = None,
    property: Option[String] = None
  ) extends Statement

  case class ScalaFile(path: Path, content: String)

  case class PackageDeclaration(name: String) extends Statement

  case class Import(name: String) extends Statement

  case class PackageDef(name: String, body: Seq[Statement]) extends Statement

  case class StatementsGroup(statements: Seq[Statement]) extends Statement {
    def +(addedStatement: Statement): StatementsGroup = plus(addedStatement)
    def plus(addedStatement: Statement): StatementsGroup = {
      StatementsGroup(statements :+ addedStatement)
    }

    def +(addedStatement: Option[Statement]): StatementsGroup = plus(addedStatement)
    def plus(addedStatement: Option[Statement]): StatementsGroup = {
      StatementsGroup(statements ++ addedStatement)
    }

    def +(addedStatements: Seq[Statement]): StatementsGroup = plus(addedStatements)
    def plus(addedStatements: Seq[Statement]): StatementsGroup = {
      StatementsGroup(statements ++ addedStatements)
    }

    def +(addedStatementGroup: StatementsGroup): StatementsGroup = plus(addedStatementGroup)
    def plus(addedStatementGroup: StatementsGroup): StatementsGroup = {
      StatementsGroup(statements ++ addedStatementGroup.statements)
    }
  }

  object StatementsGroup {
    def apply(statements: Statement*)(implicit dummyImplicit: DummyImplicit): StatementsGroup = new StatementsGroup(statements)
    def apply(statement: Option[Statement]): StatementsGroup = new StatementsGroup(statement.toSeq)
  }

  case class Val(name: String, value: Expression, isLazy: Boolean = false) extends Statement

  case class TraitDef(name: String, body: Seq[Statement], isSealed: Boolean = false) extends Statement

  case class CaseClassDef(
    name: String,
    parameters: Seq[Parameter],
    extendz: Option[String] = None,
    generics: Seq[String] = Seq.empty,
    body: Seq[Statement] = Seq.empty,
    property: Option[String] = None
  ) extends Statement

  case class ClassDef(name: String, extendz: Option[String], parameters: Seq[Parameter], body: Seq[Statement], property: Option[String], privateConstructor: Boolean) extends Statement

  case class ClassVal(name: String, typ: String, body: Seq[Statement], isLazy: Boolean = false, isPrivate: Boolean = false) extends Statement

  case class TypeDef(name: String, typ: String) extends Statement

  case class Case(pattern: String, body: Statement)
  case class Match(expr: Expression, cases: Seq[Case]) extends Statement

  case class ObjectDef(name: String, body: Seq[Statement]) extends Statement

}
