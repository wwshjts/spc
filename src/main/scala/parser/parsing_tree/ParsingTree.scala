package org.syspro.spc
package parser.parsing_tree

import org.syspro.spc.parser.parsing_tree
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

  override def kind(): SyntaxKind = ???
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
    case 3 => Some(third)
    case _ => None

  override def rank(): Int = 3
}

abstract class VarargBranch(args: ParsingTree*) extends Branch {
  override def apply(index: Int): Option[ParsingTree] = if (args.isDefinedAt(index)) Some(args(index)) else None

  override def rank(): Int = args.size
}

sealed trait Leaf extends Tree {
  override def apply(index: Int): Option[ParsingTree] = None

  override def rank(): Int = 0
}
// ---------------------------------------------

// ------------------ Grammar ------------------
sealed trait Grammar extends ParsingTree {
  override def kind(): SyntaxKind = ???
  def token(): Token = null
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

sealed trait BinaryExpr extends Expression
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
case class DOT(tkn: Token)                   extends Leaf with Terminal(tkn)
case class COLON(tkn: Token)                 extends Leaf with Terminal(tkn)
case class COMMA(tkn: Token)                 extends Leaf with Terminal(tkn)
case class PLUS(tkn: Token)                  extends Leaf with Terminal(tkn)
case class MINUS(tkn: Token)                 extends Leaf with Terminal(tkn)
case class ASTERISK(tkn: Token)              extends Leaf with Terminal(tkn)
case class SLASH(tkn: Token)                 extends Leaf with Terminal(tkn)
case class TILDE(tkn: Token)                 extends Leaf with Terminal(tkn)
case class PERCENT(tkn: Token)               extends Leaf with Terminal(tkn)
case class OPEN_PAREN(tkn: Token)            extends Leaf with Terminal(tkn)
case class CLOSE_PAREN(tkn: Token)           extends Leaf with Terminal(tkn)
case class OPEN_BRACKET(tkn: Token)          extends Leaf with Terminal(tkn)
case class CLOSE_BRACKET(tkn: Token)         extends Leaf with Terminal(tkn)
case class AMPERSAND(tkn: Token)             extends Leaf with Terminal(tkn)
case class CARET(tkn: Token)                 extends Leaf with Terminal(tkn)
case class BAR(tkn: Token)                   extends Leaf with Terminal(tkn)
case class LEFT_LEFT(tkn: Token)             extends Leaf with Terminal(tkn)
case class RIGHT_RIGHT(tkn: Token)           extends Leaf with Terminal(tkn)
case class AMPERSAND_AMPERSAND(tkn: Token)   extends Leaf with Terminal(tkn)
// TODO: add other symbols to IR

// keyword
case class THIS(tkn: Token)           extends Leaf with Terminal(tkn)
case class SUPER(tkn: Token)          extends Leaf with Terminal(tkn)
case class NULL(tkn: Token)           extends Leaf with Terminal(tkn)

// some real shit
case class SeparatedList(node: ParsingTree*)  extends VarargBranch(node*) with Grammar

abstract class LiteralExpr(terminal: Terminal) extends UnaryBranch(terminal) with Atom
/**
 * Trait which specify branch with only one descendant
 */
abstract class UnaryExpr(operation: Terminal, operand: ParsingTree) extends BinaryBranch(operation, operand) with Expression {}

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
case class IdentifierName(op: Terminal)         extends LiteralExpr(op)

case class GroupBy(leftb: Terminal, expr: ParsingTree, rightb: Terminal)              extends TernaryBranch(leftb, expr, rightb) with Primary
case class MemberAccess(left: ParsingTree, dot: Terminal, i: Terminal)                extends TernaryBranch(left, dot, i) with Primary
case class Index(indexed: ParsingTree, l: Terminal, index: ParsingTree, r: Terminal)  extends VarargBranch(indexed, l, index, r) with Primary
case class Invoke(i: ParsingTree, lp: Terminal, list: SeparatedList, rp: Terminal)    extends VarargBranch(i, lp, list, rp) with Primary

abstract class BinaryExpression(left: ParsingTree, op: Terminal, right: ParsingTree) extends TernaryBranch(left, op, right) with Expression {}

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

object BinaryExpression {
  def apply(left: ParsingTree, op: Terminal, right: ParsingTree): BinaryExpression = {
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
      case t: CARET => Xor(left, op, right)
      case t: BAR => BitwiseOr(left, op, right)
  }
}

// ============================= Syntax ==========================

/** needed for DSL */
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
case object NULL          extends DSLEntity { override def apply(tkn: Token): Terminal = new NULL(tkn) }

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
case object CLOSE_BRACKET extends Symbol
case object LEFT_LEFT     extends Symbol
case object RIGHT_RIGHT   extends Symbol

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

      case "<<" => LEFT_LEFT
      case ">>" => RIGHT_RIGHT
      case "&&" =>  AMPERSAND_AMPERSAND
  }
}