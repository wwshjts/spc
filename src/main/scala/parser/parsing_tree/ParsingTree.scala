package org.syspro.spc
package parser.parsing_tree

import syspro.tm.lexer.Token
import syspro.tm.parser.{AnySyntaxKind, SyntaxKind, SyntaxNode}


/*
 r
*/

/**
 * Represents IR of spc compiler parser
 * Every IR atom should extend ParsingTree
 *
 * IR of compiler split into two different trait families
 * This partitioning allows for a flexible internal representation
 *
 * @see Tree
 * @see Gramma
 */
sealed trait ParsingTree extends SyntaxNode {
  def kind(): SyntaxKind
  def apply(index: Int): Option[Tree]
  def rank(): Int
  def token(): Token
}

// ------------------ Tree ------------------
sealed trait Tree extends ParsingTree {
  def apply(index: Int): Option[Tree]
  def rank(): Int
}

sealed trait Branch extends Tree


sealed trait Leaf extends Tree {
  override def apply(index: Int): Option[Tree] = None

  override def rank(): Int = 0
}
// ---------------------------------------------

// ------------------ Grammar ------------------
sealed trait Grammar {
  def kind(): SyntaxKind = ???
  def token(): Token = null
}

sealed trait Terminal(tkn: Token) extends Grammar {
  override def token(): Token = tkn
}

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

/**
 * Trait which specify branch with only one descendant
 */
abstract class UnaryExpr(operation: Terminal, leaf: Leaf) extends Branch {}

/*
case class Negate(op: ParsingTree)                         extends Unary(op)
case class UPlus(op: ParsingTree)                          extends Unary(op)
case class BitwiseNot(op: ParsingTree)                     extends Unary(op)

case class StringLiteral(op: Leaf)        extends Unary(op)
case class IntegerLiteral(op: Leaf)       extends Unary(op)
case class RuneLiteral(op: Leaf)          extends Unary(op)
case class BooleanLiteral(op: Leaf)       extends Unary(op)

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
sealed trait Syntax {
  // TODO: def kind(): AnySyntaxKind

  // TODO
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
}

case object INDENT        extends Syntax
case object DEDENT        extends Syntax

case object RUNE          extends Syntax
case object IDENTIFIER    extends Syntax

case object BAD           extends Syntax

sealed trait Symbol       extends Syntax
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
sealed trait BuiltInType  extends Syntax
case object INTEGER       extends BuiltInType
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