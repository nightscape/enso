package org.enso.interpreter

import java.util.Optional
import fastparse._, ScalaWhitespace._
import scala.collection.JavaConverters._

trait AstExpressionVisitor[+T] {
  def visitLong(l: Long): T

  def visitArithOp(op: String, left: AstExpression, right: AstExpression): T

  def visitForeign(lang: String, code: String): T

  def visitVariable(name: String): T

  def visitFunction(
    arguments: java.util.List[AstArgDefinition],
    statements: java.util.List[AstExpression],
    retValue: AstExpression
  ): T

  def visitCaseFunction(
    arguments: java.util.List[AstArgDefinition],
    statements: java.util.List[AstExpression],
    retValue: AstExpression
  ): T

  def visitFunctionApplication(
    function: AstExpression,
    arguments: java.util.List[AstCallArg],
    defaultsSuspended: Boolean
  ): T

  def visitIf(
    cond: AstExpression,
    ifTrue: AstExpression,
    ifFalse: AstExpression
  ): T

  def visitAssignment(varName: String, expr: AstExpression): T

  def visitPrint(body: AstExpression): T

  def visitMatch(
    target: AstExpression,
    branches: java.util.List[AstCase],
    fallback: java.util.Optional[AstCaseFunction]
  ): T
}

trait AstGlobalScopeVisitor[+T] {

  @throws(classOf[Exception])
  def visitGlobalScope(
    imports: java.util.List[AstImport],
    typeDefs: java.util.List[AstTypeDef],
    bindings: java.util.List[AstMethodDef],
    expression: AstExpression
  ): T
}

sealed trait AstGlobalSymbol

case class AstTypeDef(name: String, arguments: List[AstArgDefinition])
    extends AstGlobalSymbol {
  def getArguments: java.util.List[AstArgDefinition] = arguments.asJava
}

case class AstMethodDef(typeName: String, methodName: String, fun: AstFunction)
    extends AstGlobalSymbol

case class AstImport(name: String)

case class AstGlobalScope(
  imports: List[AstImport],
  bindings: List[AstGlobalSymbol],
  expression: AstExpression) {

  def visit[T](visitor: AstGlobalScopeVisitor[T]): T = {
    val types = new java.util.ArrayList[AstTypeDef]()
    val defs  = new java.util.ArrayList[AstMethodDef]()

    bindings.foreach {
      case assignment: AstMethodDef => defs.add(assignment)
      case typeDef: AstTypeDef      => types.add(typeDef)
    }

    visitor.visitGlobalScope(imports.asJava, types, defs, expression)
  }
}

sealed trait AstExpression {
  def visit[T](visitor: AstExpressionVisitor[T]): T
}

sealed trait AstArgDefinition {
  def visit[T](visitor: AstArgDefinitionVisitor[T], position: Int): T
}

trait AstArgDefinitionVisitor[+T] {
  def visitDefaultedArg(name: String, value: AstExpression, position: Int): T

  def visitBareArg(name: String, position: Int): T
}

case class AstDefaultedArgDefinition(name: String, value: AstExpression)
    extends AstArgDefinition {
  override def visit[T](visitor: AstArgDefinitionVisitor[T], position: Int): T =
    visitor.visitDefaultedArg(name, value, position)
}

case class AstBareArgDefinition(name: String) extends AstArgDefinition {
  override def visit[T](visitor: AstArgDefinitionVisitor[T], position: Int): T =
    visitor.visitBareArg(name, position)
}

sealed trait AstCallArg {
  def visit[T](visitor: AstCallArgVisitor[T], position: Int): T
}

trait AstCallArgVisitor[+T] {
  def visitNamedCallArg(name: String, value: AstExpression, position: Int): T

  def visitUnnamedCallArg(value: AstExpression, position: Int): T
}

case class AstNamedCallArg(name: String, value: AstExpression)
    extends AstCallArg {
  override def visit[T](visitor: AstCallArgVisitor[T], position: Int): T =
    visitor.visitNamedCallArg(name, value, position)
}

case class AstUnnamedCallArg(value: AstExpression) extends AstCallArg {
  override def visit[T](visitor: AstCallArgVisitor[T], position: Int): T =
    visitor.visitUnnamedCallArg(value, position)
}

case class AstLong(l: Long) extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitLong(l)
}

case class AstArithOp(op: String, left: AstExpression, right: AstExpression)
    extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitArithOp(op, left, right)
}

case class AstForeign(lang: String, code: String) extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitForeign(lang, code)
}

case class AstVariable(name: String) extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitVariable(name)
}

case class AstApply(
  fun: AstExpression,
  args: List[AstCallArg],
  hasDefaultsSuspended: Boolean)
    extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitFunctionApplication(fun, args.asJava, hasDefaultsSuspended)
}

case class AstFunction(
  arguments: List[AstArgDefinition],
  statements: List[AstExpression],
  ret: AstExpression)
    extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitFunction(arguments.asJava, statements.asJava, ret)

  def getArguments: java.util.List[AstArgDefinition] = arguments.asJava

  def getStatements: java.util.List[AstExpression] = statements.asJava
}

case class AstCaseFunction(
  arguments: List[AstArgDefinition],
  statements: List[AstExpression],
  ret: AstExpression)
    extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitCaseFunction(arguments.asJava, statements.asJava, ret)
}

case class AstAssignment(name: String, body: AstExpression)
    extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitAssignment(name, body)
}

case class AstPrint(body: AstExpression) extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitPrint(body)
}

case class AstIfZero(
  cond: AstExpression,
  ifTrue: AstExpression,
  ifFalse: AstExpression)
    extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitIf(cond, ifTrue, ifFalse)
}

case class AstCase(cons: AstExpression, function: AstCaseFunction)

case class AstMatch(
  target: AstExpression,
  branches: Seq[AstCase],
  fallback: Option[AstCaseFunction])
    extends AstExpression {
  override def visit[T](visitor: AstExpressionVisitor[T]): T =
    visitor.visitMatch(
      target,
      branches.asJava,
      Optional.ofNullable(fallback.orNull)
    )
}

class EnsoParserInternal {

  def nonEmptyList[T, _ : P](parser: P[T]): P[List[T]] =
    parser.rep(1, sep = ",").map(_.toList)

  def wholeNumber[_: P]: P[Long] = P(CharIn("0-9").rep(1).!.map(_.toLong))
  def long[_ : P]: P[AstLong] = wholeNumber.map(AstLong(_))

  def foreign[_ : P]: P[AstForeign] =
    ("js" | "rb" | "py").! ~ foreignLiteral map {
      case (lang, code) => AstForeign(lang, code)
    }

  def argList[_ : P]: P[List[AstCallArg]] =
    P("[" ~ nonEmptyList(namedCallArg | unnamedCallArg) ~ "]")

  def ident[_ : P]: P [String] =
    (CharPred(Character.isJavaIdentifierStart) ~~ CharsWhile(Character.isJavaIdentifierPart(_: Char))).!.opaque("identifier") map (_.mkString)

  def namedCallArg[_ : P]: P[AstNamedCallArg] = (ident ~ "=" ~ expression) map {
    case (name, expr) => AstNamedCallArg(name, expr)
  }

  def unnamedCallArg[_ : P]: P[AstCallArg] = expression map AstUnnamedCallArg

  def defaultedArgDefinition[_ : P]: P[AstDefaultedArgDefinition] =
    (ident ~ "=" ~ expression) map {
      case (name, value) => AstDefaultedArgDefinition(name, value)
    }

  def bareArgDefinition[_ : P]: P[AstBareArgDefinition] = ident map AstBareArgDefinition

  def inArgList[_ : P]: P[List[AstArgDefinition]] =
    P("|" ~ nonEmptyList(defaultedArgDefinition | bareArgDefinition) ~ "|")

  def foreignLiteral[_ : P]: P[String] = "**" ~ CharsWhile(_ != '*').! ~ "**"

  def variable[_ : P]: P[AstVariable] = ident map AstVariable

  def operand[_ : P]: P[AstExpression] =
    long | foreign | variable | "(" ~ expression ~ ")" | functionCall

  def arith[_ : P]: P[AstExpression] =
    (operand ~ (("+" | "-" | "*" | "/" | "%").! ~ operand).?) map {
      case (a, Some((op, b))) => AstArithOp(op, a, b)
      case (a, None) => a
    }

  def expression[_ : P]: P[AstExpression] =
    ifZero | matchClause | arith | function

  def functionCall[_ : P]: P[AstApply] =
    "@" ~ expression ~ argList.? ~ defaultSuspend map {
      case (expr, args, hasDefaultsSuspended) =>
        AstApply(expr, args.getOrElse(Nil), hasDefaultsSuspended)
    }

  def defaultSuspend[_ : P]: P[Boolean] =
    ("...".!.?) map ({
      case Some(_) => true
      case None    => false
    })

  def assignment[_ : P]: P[AstAssignment] = (ident ~ "=" ~ expression) map {
    case (v, exp) => AstAssignment(v, exp)
  }

  def print[_ : P]: P[AstPrint] = P("print:" ~ expression) map AstPrint

  def ifZero[_ : P]: P[AstIfZero] =
    P("ifZero:" ~ "[" ~ (expression ~ ("," ~ expression ~ ("," ~ expression))) ~ "]") map {
      case (cond, (ift, iff)) => AstIfZero(cond, ift, iff)
    }

  def function[_ : P]: P[AstFunction] =
    P("{" ~/ inArgList.? ~ statement.rep(sep = ";") ~ expression ~ "}") map {
      case (args, stmts, expr) => AstFunction(args.getOrElse(Nil), stmts.toList, expr)
    }

  def caseFunction[_ : P]: P[AstCaseFunction] = function map {
    case AstFunction(args, stmts, ret) => AstCaseFunction(args, stmts, ret)
  }

  def caseClause[_ : P]: P[AstCase] =
    P(expression ~ "~" ~ caseFunction ~ ";") map {
      case (cons, fun) =>
        AstCase(cons, AstCaseFunction(fun.arguments, fun.statements, fun.ret))
    }

  def matchClause[_ : P]: P[AstMatch] =
    P("match" ~ expression ~ "<" ~ caseClause.rep ~ (caseFunction ~ ";").? ~ ">") map {
      case (expr, cases, fallback) => AstMatch(expr, cases, fallback)
    }

  def statement[_ : P]: P[AstExpression] = assignment | print | expression

  def typeDef[_ : P]: P[AstGlobalSymbol] =
    P("type" ~ ident ~ ((bareArgDefinition | ("(" ~ defaultedArgDefinition ~ ")")).rep) ~ ";") map {
      case (name, args) => AstTypeDef(name, args.toList)
    }

  def methodDef[_ : P]: P[AstMethodDef] =
    P(ident ~~ "." ~~/ ident ~ "=" ~ expression).map {
      case (typeName, methodName, body) =>
        val thisArg = AstBareArgDefinition(Constants.THIS_ARGUMENT_NAME);
        val fun = body match {
          case b: AstFunction =>
            b.copy(arguments = thisArg :: b.arguments)
          case _ => AstFunction(List(thisArg), List(), body)
        }
        AstMethodDef(typeName, methodName, fun)
    }.log

  def importStmt[_ : P]: P[AstImport] =
    P("import" ~ ident.repX(sep = ".")) map (segs => AstImport(segs.mkString(".")))

  def globalScope[_ : P]: P[AstGlobalScope] =
    P(importStmt.rep ~ (typeDef | methodDef).rep ~ expression) map {
      case (imports, assignments, expr) =>
        AstGlobalScope(imports.toList, assignments.toList, expr)
    }

  def parseGlobalScope(code: String): AstGlobalScope = {
    val Parsed.Success(value, _) = fastparse.parse(code, globalScope(_))
    value
  }

  def expr[_ : P]: P[AstExpression] = P(expression | function)
  def parse(code: String): AstExpression = {
    val Parsed.Success(value, _) = fastparse.parse(code, expr(_))
    value
  }
}

object ParserTest extends App {
  println(parse("Unit.testM = 1", new EnsoParserInternal().methodDef(_)))
  println(parse("Unit", new EnsoParserInternal().ident(_)))
  println(Character.isJavaIdentifierStart('U'))
}

class EnsoParser {

  def parseEnso(code: String): AstGlobalScope = {
    new EnsoParserInternal().parseGlobalScope(code)
  }
}
