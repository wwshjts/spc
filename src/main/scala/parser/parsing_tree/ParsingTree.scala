package org.syspro.spc
package parser.parsing_tree

import org.syspro.spc.parser.parsing_tree
import org.syspro.spc.parser.token.ParsingTreeConverter
import org.w3c.dom.css.Counter
import syspro.tm.lexer.Token
import syspro.tm.parser.{AnySyntaxKind, SyntaxKind, SyntaxNode}


/**
 * Represents IR of spc compiler parser
 * Every IR atom should extend ParsingTree
 *
 * IR of compiler split into two different trait families
 * This partitioning allows for a flexible internal representation
 *
 * @see Tree
 * @see Grammar
 */
sealed trait ParsingTree extends SyntaxNode {
  def apply(index: Int): Option[ParsingTree]
  def rank(): Int
  def token(): Token

  override def kind(): AnySyntaxKind = ParsingTreeConverter(this)
  override def slotCount(): Int = rank()
  override def slot(i: Int): SyntaxNode = apply(i).get
}

// ------------------ Tree ------------------
sealed trait Tree extends ParsingTree {
  def apply(index: Int): Option[ParsingTree]
  def rank(): Int
}

sealed trait Branch extends Tree

sealed trait UnaryBranch(branch: ParsingTree) extends Branch {
  override def apply(index: Int): Option[ParsingTree] = index match
    case 0 => Some(branch)
    case _ => None

  override def rank() = 1
}

sealed trait BinaryBranch(left: ParsingTree, right: ParsingTree) extends Branch {
  override def apply(index: Int): Option[ParsingTree] = index match
    case 0 => Some(left)
    case 1 => Some(right)
    case _ => None

  override def rank() = 2
}

sealed trait TernaryBranch(first: ParsingTree, second: ParsingTree, third: ParsingTree) extends Branch {
  override def apply(index: Int): Option[ParsingTree] = index match
    case 0 => Some(first)
    case 1 => Some(second)
    case 2 => Some(third)
    case _ => None

  override def rank(): Int = 3
}

abstract class VarargBranch(args: ParsingTree*) extends Branch {
  override def apply(index: Int): Option[ParsingTree] = if (args.isDefinedAt(index)) Some(args(index)) else None

  override def rank(): Int = args.size
}

abstract class ListVararg(args: List[ParsingTree]) extends Branch {
  override def apply(index: Int): Option[ParsingTree] = if (args.isDefinedAt(index)) Some(args(index)) else None

  override def rank(): Int = args.size
}

object ListVararg {
  def optionalVarargs(opt: Option[ParsingTree]*):List[ParsingTree] = opt.toList.flatMap {
    case Some(value) => List(value)
    case None => List.empty
  }

}

sealed trait Leaf extends Tree {
  override def apply(index: Int): Option[ParsingTree] = None

  override def rank(): Int = 0
}
// ---------------------------------------------

// ------------------ Grammar ------------------
sealed trait Grammar extends ParsingTree {
  override def token(): Token = null
}

/**
 * Represents Terminal symbols of the grammar
 * Also Terminal Nodes is elements of DSL Language of ParserCombinatorLib
 */
sealed trait Terminal(tkn: Token) extends Grammar {
  override def token(): Token = tkn
}

sealed trait Expression extends Grammar

sealed trait Atom extends Primary

sealed trait Primary extends Expression

sealed trait Name extends Primary

sealed trait Statement extends Grammar

sealed trait Definition extends Grammar

/**
 *
 * Should represent supplementary grammar rules such as blocks
 *
 */
sealed trait Supplementary extends Grammar {
  override def kind(): SyntaxKind = throw new IllegalAccessException("Supplementary node doesn't correspond to syntax kind")
}

// ---------------------------------------------


case class BAD(tkn: Token)          extends Leaf with Terminal(tkn)

// Indentation and other syntax
case class INDENT(tkn: Token)       extends Leaf with Terminal(tkn)
case class DEDENT(tkn: Token)       extends Leaf with Terminal(tkn)

case class IDENTIFIER(tkn: Token)   extends Leaf with Terminal(tkn)
case class RUNE(tkn: Token)         extends Leaf with Terminal(tkn)

// Built-in types
case class BOOLEAN(tkn: Token)      extends Leaf with Terminal(tkn)
case class INTEGER(tkn: Token)      extends Leaf with Terminal(tkn)
case class STRING(tkn: Token)       extends Leaf with Terminal(tkn)

// Symbols
case class DOT(tkn: Token)                    extends Leaf with Terminal(tkn)
case class COLON(tkn: Token)                  extends Leaf with Terminal(tkn)
case class COMMA(tkn: Token)                  extends Leaf with Terminal(tkn)
case class PLUS(tkn: Token)                   extends Leaf with Terminal(tkn)
case class MINUS(tkn: Token)                  extends Leaf with Terminal(tkn)
case class ASTERISK(tkn: Token)               extends Leaf with Terminal(tkn)
case class SLASH(tkn: Token)                  extends Leaf with Terminal(tkn)
case class TILDE(tkn: Token)                  extends Leaf with Terminal(tkn)
case class PERCENT(tkn: Token)                extends Leaf with Terminal(tkn)
case class OPEN_PAREN(tkn: Token)             extends Leaf with Terminal(tkn)
case class CLOSE_PAREN(tkn: Token)            extends Leaf with Terminal(tkn)
case class OPEN_BRACKET(tkn: Token)           extends Leaf with Terminal(tkn)
case class CLOSE_BRACKET(tkn: Token)          extends Leaf with Terminal(tkn)
case class AMPERSAND(tkn: Token)              extends Leaf with Terminal(tkn)
case class CARET(tkn: Token)                  extends Leaf with Terminal(tkn)
case class BAR(tkn: Token)                    extends Leaf with Terminal(tkn)
case class QUESTION(tkn: Token)               extends Leaf with Terminal(tkn)
case class LEFT(tkn: Token)                   extends Leaf with Terminal(tkn)
case class RIGHT(tkn: Token)                  extends Leaf with Terminal(tkn)
case class EQ(tkn: Token)                     extends Leaf with Terminal(tkn)
case class EQ_EQ(tkn: Token)                  extends Leaf with Terminal(tkn)
case class EXCLAMATION(tkn: Token)            extends Leaf with Terminal(tkn)
case class NEQ(tkn: Token)                    extends Leaf with Terminal(tkn)
case class LEFT_EQ(tkn: Token)                extends Leaf with Terminal(tkn)
case class RIGHT_EQ(tkn: Token)               extends Leaf with Terminal(tkn)
case class LEFT_LEFT(tkn: Token)              extends Leaf with Terminal(tkn)
case class RIGHT_RIGHT(tkn: Token)            extends Leaf with Terminal(tkn)
case class AMPERSAND_AMPERSAND(tkn: Token)    extends Leaf with Terminal(tkn)
case class BAR_BAR(tkn: Token)                extends Leaf with Terminal(tkn)
case class BOUND(tkn: Token)                  extends Leaf with Terminal(tkn)

// keyword
case class THIS(tkn: Token)           extends Leaf with Terminal(tkn)
case class SUPER(tkn: Token)          extends Leaf with Terminal(tkn)
case class NULL(tkn: Token)           extends Leaf with Terminal(tkn)
case class IS(tkn: Token)             extends Leaf with Terminal(tkn)
case class FOR(tkn: Token)            extends Leaf with Terminal(tkn)
case class WHILE(tkn: Token)          extends Leaf with Terminal(tkn)
case class IF(tkn: Token)             extends Leaf with Terminal(tkn)
case class ELSE(tkn: Token)           extends Leaf with Terminal(tkn)
case class IN(tkn: Token)             extends Leaf with Terminal(tkn)
case class BREAK(tkn: Token)          extends Leaf with Terminal(tkn)
case class CONTINUE(tkn: Token)       extends Leaf with Terminal(tkn)
case class RETURN(tkn: Token)         extends Leaf with Terminal(tkn)
case class VAR(tkn: Token)            extends Leaf with Terminal(tkn)
case class VAL(tkn: Token)            extends Leaf with Terminal(tkn)
case class DEF(tkn: Token)            extends Leaf with Terminal(tkn)
case class ABSTRACT(tkn: Token)       extends Leaf with Terminal(tkn)
case class VIRTUAL(tkn: Token)        extends Leaf with Terminal(tkn)
case class OVERRIDE(tkn: Token)       extends Leaf with Terminal(tkn)
case class NATIVE(tkn: Token)         extends Leaf with Terminal(tkn)
case class CLASS(tkn: Token)          extends Leaf with Terminal(tkn)
case class OBJECT(tkn: Token)         extends Leaf with Terminal(tkn)
case class INTERFACE(tkn: Token)      extends Leaf with Terminal(tkn)

// some real shit
sealed abstract class ProtoList(trees: List[ParsingTree]) extends ListVararg(trees) with Grammar
case class SeparatedList private (trees: List[ParsingTree]) extends ProtoList(trees)

// It is done only for checker module compatibility
case object SeparatedList {
  def apply(trees: List[ParsingTree]): SeparatedList = if trees.nonEmpty then new SeparatedList(trees) else null
}
case class GrammarList private (trees: List[ParsingTree])   extends ProtoList(trees)

// It is done only for checker module compatibility
case object GrammarList {
  def apply(trees: List[ParsingTree]): GrammarList = if trees.nonEmpty then new GrammarList(trees) else null
}
case class TypeBound(bound: Terminal, separatedList: SeparatedList) extends BinaryBranch(bound, separatedList) with Grammar

sealed abstract class LiteralExpr(terminal: Terminal) extends UnaryBranch(terminal) with Atom
/**
 * Trait which specify branch with only one descendant
 */
sealed abstract class UnaryExpr(operation: Terminal, operand: ParsingTree) extends BinaryBranch(operation, operand) with Expression {}

case class Negate(operation: Terminal, operand: ParsingTree)       extends UnaryExpr(operation, operand)
case class UPlus(operation: Terminal, operand: ParsingTree)        extends UnaryExpr(operation, operand)
case class BitwiseNot(operation: Terminal, operand: ParsingTree)   extends UnaryExpr(operation, operand)

case class StringLiteral(op: Terminal)          extends LiteralExpr(op)
case class IntegerLiteral(op: Terminal)         extends LiteralExpr(op)
case class RuneLiteral(op: Terminal)            extends LiteralExpr(op)
case class BooleanLiteral(op: Terminal)         extends LiteralExpr(op)

case class ThisExpr(op: Terminal)               extends LiteralExpr(op)
case class SuperExpr(op: Terminal)              extends LiteralExpr(op)
case class NullLiteral(op: Terminal)            extends LiteralExpr(op)

case class IdentifierName(op: Terminal)                 extends UnaryBranch(op) with Name
case class OptionName(op: Terminal, name: ParsingTree)  extends BinaryBranch(op, name) with Name
case class GenericName(i: Terminal, l: Terminal, separatedList: SeparatedList, r: Terminal) extends VarargBranch(i, l, separatedList, r) with Name

case class GroupBy(leftb: Terminal, expr: ParsingTree, rightb: Terminal)              extends TernaryBranch(leftb, expr, rightb) with Primary
case class MemberAccess(left: ParsingTree, dot: Terminal, i: Terminal)                extends TernaryBranch(left, dot, i) with Primary
case class Index(indexed: ParsingTree, l: Terminal, index: ParsingTree, r: Terminal)  extends VarargBranch(indexed, l, index, r) with Primary
case class Invoke(i: ParsingTree, lp: Terminal, list: SeparatedList, rp: Terminal)    extends VarargBranch(i, lp, list, rp) with Primary

case class IsExpression private (args: ParsingTree*) extends VarargBranch(args*) with Expression
case object IsExpression {
  def apply(expr: ParsingTree, is: Terminal, name_expr: Name) = new IsExpression(expr, is, name_expr)
  def apply(expr: ParsingTree, is: Terminal, name_expr: Name, opt: Terminal) = new IsExpression(expr, is, name_expr, opt)
}

sealed abstract class BinaryExpression(left: ParsingTree, op: Terminal, right: ParsingTree) extends TernaryBranch(left, op, right) with Expression {}

case class ADD(left: ParsingTree, op: Terminal, right: ParsingTree)       extends BinaryExpression(left, op, right)
case class SUBTRACT(left: ParsingTree, op: Terminal, right: ParsingTree)  extends BinaryExpression(left, op, right)

case class MULTIPLY(left: ParsingTree, op: Terminal, right: ParsingTree)  extends BinaryExpression(left, op, right)
case class DIV(left: ParsingTree, op: Terminal, right: ParsingTree)       extends BinaryExpression(left, op, right)
case class MOD(left: ParsingTree, op: Terminal, right: ParsingTree)       extends BinaryExpression(left, op, right)

case class LEFT_SHIFT(left: ParsingTree, op: Terminal, right: ParsingTree)  extends BinaryExpression(left, op, right)
case class RIGHT_SHIFT(left: ParsingTree, op: Terminal, right: ParsingTree) extends BinaryExpression(left, op, right)

case class BitwiseAnd(left: ParsingTree, op: Terminal, right: ParsingTree)  extends BinaryExpression(left, op, right)
case class BitwiseOr(left: ParsingTree, op: Terminal, right: ParsingTree)   extends BinaryExpression(left, op, right)
case class Xor(left: ParsingTree, op: Terminal, right: ParsingTree)         extends BinaryExpression(left, op, right)

case class AND(left: ParsingTree, op: Terminal, right: ParsingTree)         extends BinaryExpression(left, op, right)
case class OR(left: ParsingTree, op: Terminal, right: ParsingTree)          extends BinaryExpression(left, op, right)
case class NOT(ex: Terminal, operand: ParsingTree)                          extends UnaryExpr(ex, operand)

case class LessThan(left: ParsingTree, op: Terminal, right: ParsingTree)    extends BinaryExpression(left, op, right)
case class GreaterThan(left: ParsingTree, op: Terminal, right: ParsingTree) extends BinaryExpression(left, op, right)
case class LessOrEq(left: ParsingTree, op: Terminal, right: ParsingTree)    extends BinaryExpression(left, op, right)
case class GreaterOrEq(left: ParsingTree, op: Terminal, right: ParsingTree) extends BinaryExpression(left, op, right)
case class Equal(left: ParsingTree, op: Terminal, right: ParsingTree)       extends BinaryExpression(left, op, right)
case class NEqual(left: ParsingTree, op: Terminal, right: ParsingTree)      extends BinaryExpression(left, op, right)

object BinaryExpression {
  def apply(left: ParsingTree, op: Terminal, right: ParsingTree): Expression = {
    op match
      case t: PLUS        => ADD(left, op, right)
      case t: MINUS       => SUBTRACT(left, op, right)
      case t: ASTERISK    => MULTIPLY(left, op, right)
      case t: SLASH       => DIV(left, op, right)
      case t: PERCENT     => MOD(left, op, right)
      case t: AMPERSAND   => BitwiseAnd(left, op, right)
      case t: LEFT_LEFT   => LEFT_SHIFT(left, op, right)
      case t: RIGHT_RIGHT => RIGHT_SHIFT(left, op, right)
      case t: AMPERSAND_AMPERSAND => AND(left, op, right)
      case t: BAR_BAR => OR(left, op, right)
      case t: CARET => Xor(left, op, right)
      case t: BAR => BitwiseOr(left, op, right)

      case t: LEFT => LessThan(left, op, right)
      case t: RIGHT => GreaterThan(left, op, right)
      case t: EQ_EQ => Equal(left, op, right)
      case t: NEQ => NEqual(left, op, right)
      case t: LEFT_EQ => LessOrEq(left, op, right)
      case t: RIGHT_EQ => GreaterOrEq(left, op, right)
  }
}

// Statements
case class BreakStmt(op: Terminal) extends UnaryBranch(op) with Statement

case class ContinueStmt(op: Terminal) extends UnaryBranch(op) with Statement

case class ReturnStmt private (ret: Terminal, ret_val: Expression) extends VarargBranch(ret, ret_val) with Statement

case object ReturnStmt {
  def apply(ret: Terminal, ret_val_opt: Option[Expression]): Statement = new ReturnStmt(ret, ret_val_opt.orNull)
}

case class ExprStmt(expr: Expression) extends UnaryBranch(expr) with Statement

case class Assignment(left: Primary, op: Terminal, right: Expression) extends VarargBranch(left, op, right) with Statement

// Supplementary nodes
case class Block(indent: Terminal, body: GrammarList, dedent: Terminal) extends TernaryBranch(indent, body, dedent) with Supplementary {
  def toList: List[ParsingTree] = indent :: body :: dedent :: Nil
  def asTuple: (Terminal, GrammarList, Terminal) = (indent, body, dedent)
}

/** Not really suitable node for my IR, needed for compatibility with testing module */
object OptionalArgConverter {
  def orNull[A <: ParsingTree, B <: ParsingTree](args: Option[(A, B)]): (A, B) =
    if (args.isEmpty)
      (null.asInstanceOf[A], null.asInstanceOf[B])
    else
      (args.get._1, args.get._2)

  def orNull(b: Option[Block]): (Terminal, GrammarList, Terminal) =
    if (b.isEmpty)
      (null, null, null)
    else
      b.get.asTuple
}


case class ForStmt private (args: List[ParsingTree]) extends ListVararg(args) with Statement
object ForStmt {

  def apply(fr: Terminal, elem: Primary, in: Terminal, collection: Expression, stmt_block: Option[Block]): Statement = {

    val s: List[ParsingTree] = fr :: elem :: in :: collection :: Nil
    val (indent, body, dedent) = OptionalArgConverter.orNull(stmt_block)

    new ForStmt(fr :: elem :: in :: collection :: indent :: body :: dedent :: Nil)
  }
}

case class WhileStmt private (args: List[ParsingTree]) extends ListVararg(args) with Statement
object WhileStmt {
  def apply(cycle: Terminal, condition: Expression, stmt_block: Option[Block]): Statement = {

    val (indent, body, dedent) = OptionalArgConverter.orNull(stmt_block)

    WhileStmt(cycle :: condition :: indent :: body :: dedent :: Nil)
  }
}

case class IfStmt private (args: List[ParsingTree]) extends ListVararg(args) with Statement
object IfStmt {

  def apply(if_lit: Terminal, cond: Expression, if_block: Option[Block], else_block: Option[(Terminal, Option[Block])]): IfStmt = {
    val (if_indent, if_body, if_dedent) = OptionalArgConverter.orNull(if_block)
    val tp: (Terminal, Terminal, GrammarList, Terminal) = else_block match {
      case Some(value) =>
        val (indent, body, dedent) = OptionalArgConverter.orNull(value._2)
        (value._1, indent, body, dedent)
      case None => (null, null, null, null)
    }
    val (else_kw, else_indent, else_body, else_dedent) = tp
    IfStmt(if_lit :: cond :: if_indent :: if_body :: if_dedent :: else_kw :: else_indent :: else_body :: else_dedent :: Nil)
  }

}

case class VarDefStmt (variableDef: VariableDef) extends UnaryBranch(variableDef) with Statement

// Definitions
case class VariableDef private (args: List[ParsingTree]) extends ListVararg(args) with Definition

object VariableDef {
  // (VAR | VAL) IDENTIFIER COLON? NameExpression? EQUALS? Expression?
  def apply(mod: Terminal, identifier: Terminal, type_opt: Option[(Terminal, Name)], assignment_opt: Option[(Terminal, Expression)]): VariableDef = {
    val (colon, type_name) = OptionalArgConverter.orNull(type_opt)
    val (eq, assignment) = OptionalArgConverter.orNull(assignment_opt)

    VariableDef(mod :: identifier :: colon :: type_name :: eq :: assignment :: Nil)
  }
}

case class ParameterDef(identifier: Terminal, colon: Terminal, name: Name) extends TernaryBranch(identifier, colon, name) with Definition

case class TypeParamDef private (args: List[ParsingTree]) extends ListVararg(args) with Definition

object TypeParamDef {
  def apply(identifier: Terminal, typeBound: Option[TypeBound]): TypeParamDef = TypeParamDef(identifier :: typeBound.orNull :: Nil)
}

case class FunctionDef private (args: List[ParsingTree]) extends ListVararg(args) with Definition

object FunctionDef {
  def apply(modifiers: GrammarList, definition: Terminal, name: Terminal, lp: Terminal, args_opt: Option[SeparatedList], rp: Terminal, ret_type_opt: Option[(Terminal, Name)], block: Option[Block]): FunctionDef = {

    val args_list = args_opt.orNull
    val (ret_kw, ret_type) = OptionalArgConverter.orNull(ret_type_opt)
    val (indent, body, dedent) = OptionalArgConverter.orNull(block)

    FunctionDef(modifiers :: definition :: name :: lp :: args_list :: rp :: ret_kw :: ret_type :: indent :: body :: dedent :: Nil)
  }
}

case class TypeDefinition private (args: List[ParsingTree]) extends ListVararg(args) with Definition

object TypeDefinition {
  // TODO: see todo in grammar on this rule
  def apply(mod: Terminal, identifier: Terminal, params_opt: List[ParsingTree], bound_opt: Option[TypeBound], block: Option[Block]): TypeDefinition = {

    var less: ParsingTree = null
    var params: ParsingTree = null
    var greater: ParsingTree = null

    if (params_opt.nonEmpty)
      less = params_opt(0)
      params = params_opt(1)
      greater = params_opt(2)

    val bound = bound_opt.orNull
    val (indent, body, dedent) = OptionalArgConverter.orNull(block)

    TypeDefinition(mod :: identifier :: less :: params :: greater :: bound :: indent :: body :: dedent :: Nil)
  }
}

case class SourceText(text: GrammarList) extends Grammar with UnaryBranch(text)

// ============================= Syntax ==========================

trait DSLEntity {
  // TODO: def kind(): AnySyntaxKind
  def apply(tkn: Token): Terminal

  def of(token: Token): Terminal = {
    this match {
      case INDENT => INDENT(token)
      case DEDENT => DEDENT(token)
      case RUNE => RUNE(token)
      case IDENTIFIER => IDENTIFIER(token)
      case BAD => BAD(token)
      case NULL => BAD(token)
      case symbol: Symbol => symbol match {
        case DOT => DOT(token)
        case COLON => COLON(token)
        case COMMA => COMMA(token)
        case PLUS => PLUS(token)
        case MINUS => MINUS(token)
        case ASTERISK => ASTERISK(token)
        case SLASH => SLASH(token)
        case TILDE => TILDE(token)
        case PERCENT => PERCENT(token)
      }
      case t: BuiltInType => t match
        case INTEGER => INTEGER(token)
        case BOOLEAN => BOOLEAN(token)
        case STRING => STRING(token)
    }

  }
}

case object INDENT        extends DSLEntity { override def apply(tkn: Token): Terminal = new INDENT(tkn) }
case object DEDENT        extends DSLEntity { override def apply(tkn: Token): Terminal = new DEDENT(tkn) }
case object RUNE          extends DSLEntity { override def apply(tkn: Token): Terminal = new RUNE(tkn) }
case object IDENTIFIER    extends DSLEntity { override def apply(tkn: Token): Terminal = new IDENTIFIER(tkn) }
case object BAD           extends DSLEntity { override def apply(tkn: Token): Terminal = new BAD(tkn) }

// Keyword
case object THIS          extends DSLEntity { override def apply(tkn: Token): Terminal = new THIS(tkn) }
case object SUPER         extends DSLEntity { override def apply(tkn: Token): Terminal = new SUPER(tkn) }
case object IS            extends DSLEntity { override def apply(tkn: Token): Terminal = new IS(tkn) }
case object NULL          extends DSLEntity { override def apply(tkn: Token): Terminal = new NULL(tkn) }
case object FOR           extends DSLEntity { override def apply(tkn: Token): Terminal = new FOR(tkn) }
case object WHILE         extends DSLEntity { override def apply(tkn: Token): Terminal = new WHILE(tkn) }
case object IF            extends DSLEntity { override def apply(tkn: Token): Terminal = new IF(tkn) }
case object ELSE          extends DSLEntity { override def apply(tkn: Token): Terminal = new ELSE(tkn) }
case object IN            extends DSLEntity { override def apply(tkn: Token): Terminal = new IN(tkn) }
case object BREAK         extends DSLEntity { override def apply(tkn: Token): Terminal = new BREAK(tkn) }
case object CONTINUE      extends DSLEntity { override def apply(tkn: Token): Terminal = new CONTINUE(tkn) }
case object RETURN        extends DSLEntity { override def apply(tkn: Token): Terminal = new RETURN(tkn) }
case object VAL           extends DSLEntity { override def apply(tkn: Token): Terminal = new VAL(tkn) }
case object VAR           extends DSLEntity { override def apply(tkn: Token): Terminal = new VAR(tkn) }
case object DEF           extends DSLEntity { override def apply(tkn: Token): Terminal = new DEF(tkn) }
case object ABSTRACT      extends DSLEntity { override def apply(tkn: Token): Terminal = new ABSTRACT(tkn) }
case object VIRTUAL       extends DSLEntity { override def apply(tkn: Token): Terminal = new VIRTUAL(tkn) }
case object OVERRIDE      extends DSLEntity { override def apply(tkn: Token): Terminal = new OVERRIDE(tkn) }
case object NATIVE        extends DSLEntity { override def apply(tkn: Token): Terminal = new NATIVE(tkn) }
case object CLASS         extends DSLEntity { override def apply(tkn: Token): Terminal = new CLASS(tkn) }
case object OBJECT        extends DSLEntity { override def apply(tkn: Token): Terminal = new OBJECT(tkn) }
case object INTERFACE     extends DSLEntity { override def apply(tkn: Token): Terminal = new INTERFACE(tkn) }

sealed trait Symbol       extends DSLEntity
case object DOT           extends Symbol
case object COLON         extends Symbol
case object COMMA         extends Symbol
case object PLUS          extends Symbol
case object MINUS         extends Symbol
case object ASTERISK      extends Symbol
case object SLASH         extends Symbol
case object TILDE         extends Symbol
case object PERCENT       extends Symbol
case object OPEN_PAREN    extends Symbol
case object CLOSE_PAREN   extends Symbol
case object OPEN_BRACKET  extends Symbol
case object AMPERSAND     extends Symbol
case object CARET         extends Symbol
case object BAR           extends Symbol
case object QUESTION      extends Symbol
case object LEFT          extends Symbol
case object RIGHT         extends Symbol
case object EXCLAMATION   extends Symbol
case object EQ            extends Symbol
case object NEQ           extends Symbol
case object EQ_EQ         extends Symbol
case object LEFT_EQ       extends Symbol
case object RIGHT_EQ      extends Symbol
case object CLOSE_BRACKET extends Symbol
case object LEFT_LEFT     extends Symbol
case object RIGHT_RIGHT   extends Symbol
case object BAR_BAR       extends Symbol
case object BOUND         extends Symbol

case object  AMPERSAND_AMPERSAND extends Symbol
// TODO: add all Symbols

// TODO: Do i really need hierachy for built-in's???
sealed trait BuiltInType  extends DSLEntity
case object INTEGER       extends BuiltInType
case object BOOLEAN       extends BuiltInType
case object STRING        extends BuiltInType

// Nonterminals

object Symbol {
  def apply(symbol: Predef.String): Symbol = {
    symbol match
      case "." => DOT
      case ":" => COLON
      case "," => COMMA
      case "+" => PLUS
      case "-" => MINUS
      case "*" => ASTERISK
      case "/" => SLASH
      case "~" => TILDE
      case "%" => PERCENT
      case "(" => OPEN_PAREN
      case ")" => CLOSE_PAREN
      case "[" => OPEN_BRACKET
      case "]" => CLOSE_BRACKET
      case "&" => AMPERSAND
      case "^" => CARET
      case "|" => BAR
      case "<" => LEFT
      case ">" => RIGHT
      case "?" => QUESTION
      case "!" => EXCLAMATION
      case "=" => EQ

      case "==" => EQ_EQ
      case "!=" => NEQ
      case "<=" => LEFT_EQ
      case ">=" => RIGHT_EQ
      case "<<" => LEFT_LEFT
      case ">>" => RIGHT_RIGHT
      case "&&" => AMPERSAND_AMPERSAND
      case "||" => BAR_BAR
      case "<:" => BOUND
  }
}