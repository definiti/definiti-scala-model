package definiti.scalamodel

import java.nio.file.Path

object ScalaAST {
  type TopLevelElement = ScalaAST.PackageElement with ScalaAST.Statement

  case class ScalaFile(path: Path, content: String)

  case class Root(
    packages: Seq[Package]
  )

  sealed trait PackageElement

  case class Package(
    name: String,
    imports: Seq[Import],
    elements: Seq[PackageElement]
  )

  object Package {
    def apply(name: String, imports: Seq[Import], elements: PackageElement*)(implicit dummyImplicit: DummyImplicit): Package = {
      new Package(name, imports, elements)
    }
  }

  case class PackageDeclaration(name: String) extends Statement

  case class Import(name: String) extends Statement

  case object Blank extends Statement

  case class PackageDef(name: String, body: Seq[Statement]) extends Statement

  case class ObjectDef(
    name: String,
    extendz: Option[Extends],
    body: Seq[Statement],
    property: Option[String]
  ) extends Statement with PackageElement

  object ObjectDef {
    def apply(name: String, statements: Statement*)(implicit dummyImplicit: DummyImplicit): ObjectDef = {
      ObjectDef(name, None, statements, None)
    }
    def apply(name: String, body: Seq[Statement]): ObjectDef = {
      ObjectDef(name, None, body, None)
    }
  }

  case class CaseClassDef(
    name: String,
    parameters: Seq[Parameter],
    extendz: Option[String] = None,
    generics: Seq[String] = Seq.empty,
    body: Seq[Statement] = Seq.empty,
    property: Option[String] = None
  ) extends Statement with PackageElement

  object CaseClassDef {
    def apply(name: String, parameters: Parameter*): CaseClassDef = {
      new CaseClassDef(name, parameters)
    }
  }

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

  object Parameter {
    def apply(name: String, typ: String, defaultValue: Expression): Parameter = {
      new Parameter(name, typ, Some(defaultValue), None)
    }
  }

  case class Lambda(parameters: Seq[Parameter], body: Expression) extends Expression

  case class CallAttribute(target: Expression, name: String) extends Expression with Unambiguous

  object CallAttribute {
    def apply(target: String, name: String): CallAttribute = new CallAttribute(SimpleExpression(target), name)
  }

  case class CallMethod(
    target: Expression,
    name: String,
    arguments: Seq[Expression] = Seq.empty,
    forceParenthesis: Boolean = false,
    generics: Seq[Type] = Seq.empty
  ) extends Expression with Unambiguous

  case class CallFunction(target: Expression, arguments: Seq[Expression]) extends Expression with Unambiguous

  object CallFunction {
    def apply(target: String, arguments: Expression*): CallFunction = {
      new CallFunction(SimpleExpression(target), arguments)
    }
  }

  case class Block(body: Seq[Statement]) extends Expression {
    def simplify: Expression = body match {
      case (head: Expression) :: Nil => head
      case _ => this
    }
  }

  object Block {
    def apply(body: Statement*)(implicit dummyImplicit: DummyImplicit): Block = new Block(body)
  }

  case class New(
    name: String,
    generics: Seq[String],
    arguments: Seq[Expression]
  ) extends Expression

  object New {
    def apply(name: String, arguments: Expression*)(implicit dummyImplicit: DummyImplicit): New = {
      new New(name, Seq.empty, arguments)
    }
  }

  case class Comment(str: String) extends Statement with PackageElement

  case class Def0(
    name: String,
    typ: String,
    generics: Seq[String] = Seq.empty,
    body: Option[Expression] = None,
    property: Option[String] = None
  ) extends Statement with PackageElement

  case class Def1(
    name: String,
    typ: String,
    generics: Seq[String] = Seq.empty,
    parameters: Seq[Parameter] = Seq.empty,
    body: Option[Expression] = None,
    property: Option[String] = None
  ) extends Statement with PackageElement

  object Def1 {
    def apply(name: String, typ: String, parameters: Seq[Parameter], body: Expression): Def1 = {
      new Def1(name, typ, Seq.empty, parameters, Some(body), None)
    }
  }

  case class Def2(
    name: String,
    typ: String,
    generics: Seq[String] = Seq.empty,
    parameters1: Seq[Parameter] = Seq.empty,
    parameters2: Seq[Parameter] = Seq.empty,
    body: Option[Expression] = None,
    property: Option[String] = None
  ) extends Statement with PackageElement

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

  case class Val(name: String, value: Expression, isLazy: Boolean = false, isImplicit: Boolean = false) extends Statement

  case class TraitDef(name: String, body: Seq[Statement], isSealed: Boolean = false) extends Statement

  case class ClassDef(name: String, generics: Seq[String], extendz: Option[Extends], parameters: Seq[Parameter], body: Seq[Statement], property: Option[String], privateConstructor: Boolean) extends Statement with PackageElement

  case class Extends(typ: Type, parameters: Seq[Statement])

  case class ClassVal(name: String, typ: String, body: Seq[Statement], isLazy: Boolean = false, isPrivate: Boolean = false, isImplicit: Boolean = false) extends Statement

  case class TypeDef(name: String, typ: String) extends Statement

  case class Case(pattern: String, body: Statement)
  case class Match(expr: Expression, cases: Seq[Case]) extends Statement

  case class Type(name: String, generics: Seq[Type] = Seq.empty) {
    // FIXME: We keep this code generation here for compatibility.
    // An improvement of the ScalaAST is needed to always work with Type and not String.
    def toCode: String = {
      if (generics.nonEmpty) {
        s"${name}[${generics.map(_.toCode).mkString(", ")}]"
      } else {
        name
      }
    }
  }
}
