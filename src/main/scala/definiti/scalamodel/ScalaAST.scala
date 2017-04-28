package definiti.scalamodel

object ScalaAST {

  sealed trait Statement

  sealed trait Expression extends Statement

  sealed trait Unambiguous // no parentheses required

  case class SimpleExpression(str: String) extends Expression with Unambiguous

  case class BinaryOp(op: String, left: Expression, right: Expression) extends Expression
  case class UnaryOp(op: String, inner: Expression) extends Expression

  sealed trait If extends Expression
  case class IfThen(cond: Expression, ifTrue: Expression) extends If
  case class IfThenElse(cond: Expression, ifTrue: Expression, ifFalse: Expression) extends If

  case class Parameter(name: String, typ: String)
  case class Lambda(parameters: Seq[Parameter], body: Expression) extends Expression

  case class CallAttribute(target: Expression, name: String) extends Expression with Unambiguous
  case class CallMethod(target: Expression, name: String, arguments: Seq[Expression]) extends Expression with Unambiguous
  case class CallFunction(target: Expression, arguments: Seq[Expression]) extends Expression with Unambiguous

  case class Block(body: Seq[Statement]) extends Expression

  case class Comment(str: String) extends Statement

  case class Def(name: String, typ: String, parameters: Seq[Parameter], body: Expression, property: Option[String]) extends Statement
  case class Def2(name: String, typ: String, parameters1: Seq[Parameter], parameters2: Seq[Parameter], body: Expression, property: Option[String]) extends Statement

  case class Import(name: String) extends Statement

  case class PackageDef(name: String, body: Seq[Statement]) extends Statement

  case class StatementsGroup(statements: Seq[Statement]) extends Statement

}
