package org.syspro.spc
package parser.parsing_tree

import org.syspro.spc.parser.DSLEntity
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
  def kind(): SyntaxKind
  def apply(index: Int): Option[Tree]
  def rank(): Int
  def token(): Token
}

// ------------------ Tree ------------------
sealed trait Tree extends ParsingTree {
  def apply(index: Int): Option[ParsingTree]
  def rank(): Int
}

sealed trait Branch extends Tree

sealed trait Unary(branch: ParsingTree) extends Branch {
  override def apply(index: Int): Option[ParsingTree] = index match
    case 0 => Some(branch)
    case _ => None

  override def rank() = 1
}

sealed trait Binary(left: ParsingTree, right: ParsingTree) extends Branch {
  override def apply(index: Int): Option[ParsingTree] = index match
    case 0 => Some(left)
    case 1 => Some(right)
    case _ => None

  override def rank() = 2
}

sealed trait Leaf extends Tree {
  override def apply(index: Int): Option[ParsingTree] = None

  override def rank(): Int = 0
}
// ---------------------------------------------

// ------------------ Grammar ------------------
sealed trait Grammar extends ParsingTree {
  def kind(): SyntaxKind = ???
  def token(): Token = null
}

/**
 * Represents Terminal symbols of the grammar
 * Also Terminal Nodes is elements of DSL Language of ParserCombinatorLib
 */
sealed trait Terminal(tkn: Token) extends Grammar with DSLEntity {
  override def token(): Token = tkn
}

sealed trait Expression

sealed trait Primary

// ---------------------------------------------


case class BAD(tkn: Token)          extends Leaf with Terminal(tkn) with Syntax

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
case class DOT(tkn: Token)          extends Leaf with Terminal(tkn)
case class COLON(tkn: Token)        extends Leaf with Terminal(tkn)
case class COMMA(tkn: Token)        extends Leaf with Terminal(tkn)
case class PLUS(tkn: Token)         extends Leaf with Terminal(tkn)
case class MINUS(tkn: Token)        extends Leaf with Terminal(tkn)
case class ASTERISK(tkn: Token)     extends Leaf with Terminal(tkn)
case class SLASH(tkn: Token)        extends Leaf with Terminal(tkn)
case class TILDE(tkn: Token)        extends Leaf with Terminal(tkn)
// TODO: add other symbols to IR

abstract class LiteralExpr(terminal: Terminal) extends Unary(terminal) with Primary

/**
 * Trait which specify branch with only one descendant
 */
abstract class UnaryExpr(operation: Terminal, operand: ParsingTree) extends Binary(operation, operand) {}

case class Negate(operation: MINUS, operand: ParsingTree)                         extends UnaryExpr(operation, operand)
case class UPlus(operation: PLUS, operand: ParsingTree)                           extends UnaryExpr(operation, operand)
case class BitwiseNot(operation: TILDE, operand: ParsingTree)                     extends UnaryExpr(operation, operand)


case class StringLiteral(op: STRING)          extends LiteralExpr(op)
case class IntegerLiteral(op: INTEGER)        extends LiteralExpr(op)
case class RuneLiteral(op: RUNE)              extends LiteralExpr(op)
case class BooleanLiteral(op: BOOLEAN)        extends LiteralExpr(op)

/*
case class GroupBy(op: ParsingTree)       extends Unary(op)


sealed trait Binary(left: ParsingTree, right: ParsingTree) extends Branch {
  override def apply(index: Int): Option[ParsingTree] = index match {
    case 0 => Some(left)
    case 1 => Some(right)
    case _ => None
  }

  override def rank(): Int = 2
}

case class ADD(left: ParsingTree, right: ParsingTree) extends Binary(left, right)
case class SUBTRACT(left: ParsingTree, right: ParsingTree) extends  Binary(left, right)
case class MULTIPLY(left: ParsingTree, right: ParsingTree) extends  Binary(left, right)
case class DIV(left: ParsingTree, right: ParsingTree) extends Binary(left, right)


 */



// ============================= Syntax ==========================

/** needed for DSL */
trait DSLEntity {
  // TODO: def kind(): AnySyntaxKind

  // TODO
  /*
  def of(token: Token): Terminal = {
    this match {
      case INDENT => INDENT(token)
      case DEDENT => DEDENT(token)
      case RUNE => RUNE(token)
      case IDENTIFIER => IDENTIFIER(token)
      case BAD => BAD(token)
      case symbol: Symbol => symbol match {
        case DOT => DOT(token)
        case COLON => COLON(token)
        case COMMA => COMMA(token)
        case PLUS => PLUS(token)
        case MINUS => MINUS(token)
        case ASTERISK => ASTERISK(token)
        case SLASH => SLASH(token)
        case TILDE => TILDE(token)
      }
      case t: BuiltInType => t match
        case INTEGER => INTEGER(token)
        case BOOLEAN => BOOLEAN(token)
        case STRING => STRING(token)
    }

  }
   */
}

case object INDENT        extends DSLEntity
case object DEDENT        extends DSLEntity

case object RUNE          extends DSLEntity
case object IDENTIFIER    extends DSLEntity

case object BAD           extends DSLEntity

sealed trait Symbol       extends DSLEntity
case object DOT           extends Symbol
case object COLON         extends Symbol
case object COMMA         extends Symbol
case object PLUS          extends Symbol
case object MINUS         extends Symbol
case object ASTERISK      extends Symbol
case object SLASH         extends Symbol
case object TILDE         extends Symbol
// TODO: add all Symbols

// TODO: Do i really need hierachy for built-in's???
sealed trait BuiltInType  extends DSLEntity
case object INTEGER       extends BuiltInType {
  override def apply(tkn: Token): INTEGER = INTEGER(tkn)
}
case object BOOLEAN       extends BuiltInType
case object STRING        extends BuiltInType

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
  }
}

