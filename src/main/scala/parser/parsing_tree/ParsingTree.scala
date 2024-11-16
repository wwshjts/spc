package org.syspro.spc
package parser.parsing_tree

import syspro.tm.lexer.Token
import syspro.tm.parser.{AnySyntaxKind, SyntaxKind, SyntaxNode}


/**
 * Represents IR of spc compiler parser
 * Every IR atom should extend ParsingTree
 */
sealed trait ParsingTree extends SyntaxNode with AnySyntaxKind {
  // every Parsing tree part should satisfy SyntaxNode contract
  override def kind(): SyntaxKind = ???
  override def slotCount(): Int = rank()
  override def slot(index: Int): SyntaxNode = apply(index).get
  override def token(): Token = null

  /**
   * Returns tree descendant by its index
   */
  def apply(index: Int): Option[ParsingTree]

  /**
   * Amount of direct descendants of tree
   */
  def rank(): Int
}

sealed trait Branch extends ParsingTree {}

sealed trait Leaf(tkn: Token) extends ParsingTree {
  override def apply(index: Int): Option[ParsingTree] = None
  override def rank(): Int = 0
  override def token(): Token = tkn
}

case class BAD(tkn: Token)          extends Leaf(tkn)

// Indentation
case class INDENT(tkn: Token)       extends Leaf(tkn)
case class DEDENT(tkn: Token)       extends Leaf(tkn)

case class IDENTIFIER(tkn: Token)   extends Leaf(tkn)
case class RUNE(tkn: Token)

// Built-in types
case class BOOLEAN(tkn: Token)      extends Leaf(tkn)
case class INTEGER(tkn: Token)      extends Leaf(tkn)
case class STRING(tkn: Token)       extends Leaf(tkn)

sealed trait Symbol {}
case class DOT(tkn: Token)          extends Symbol with Leaf(tkn)
case class COLON(tkn: Token)        extends Symbol with Leaf(tkn)
case class COMMA(tkn: Token)        extends Symbol with Leaf(tkn)
case class PLUS(tkn: Token)         extends Symbol with Leaf(tkn)
case class MINUS(tkn: Token)        extends Symbol with Leaf(tkn)
case class ASTERISK(tkn: Token)     extends Symbol with Leaf(tkn)
case class SLASH(tkn: Token)        extends Symbol with Leaf(tkn)
// TODO: add other symbols to IR

sealed trait Expression extends Branch {}
sealed trait BinaryExpression(left: Expression, right: Expression) extends Expression {
  override def apply(index: Int): Option[ParsingTree] = index match {
    case 0 => Some(left)
    case 1 => Some(right)
    case _ => None
  }

  override def rank(): Int = 2
}

case class ADD(left: Expression, right: Expression) extends BinaryExpression(left, right)
case class SUBTRACT(leaf: Expression, right: Expression) extends  BinaryExpression(leaf, right)

object Symbol {

  // TODO: turn this methods to corresponding conversions or Partial
  /**
   * Construct Symbol Leaf of ParsingTree IR
   * @param token token from Leaf constructed
   * @return
   */
  def of(token: Token): Symbol = token.toSyntaxKind.asInstanceOf[syspro.tm.lexer.Symbol] match {
    case syspro.tm.lexer.Symbol.DOT => DOT(token)
    case syspro.tm.lexer.Symbol.COLON => COLON(token)
    case syspro.tm.lexer.Symbol.COMMA => COMMA(token)
    case syspro.tm.lexer.Symbol.PLUS => PLUS(token)
    case syspro.tm.lexer.Symbol.MINUS => MINUS(token)
    case syspro.tm.lexer.Symbol.ASTERISK => ASTERISK(token)
    case syspro.tm.lexer.Symbol.SLASH => SLASH(token)
    case syspro.tm.lexer.Symbol.PERCENT => ???
    case syspro.tm.lexer.Symbol.EXCLAMATION => ???
    case syspro.tm.lexer.Symbol.TILDE => ???
    case syspro.tm.lexer.Symbol.AMPERSAND => ???
    case syspro.tm.lexer.Symbol.BAR => ???
    case syspro.tm.lexer.Symbol.AMPERSAND_AMPERSAND => ???
    case syspro.tm.lexer.Symbol.BAR_BAR => ???
    case syspro.tm.lexer.Symbol.CARET => ???
    case syspro.tm.lexer.Symbol.LESS_THAN => ???
    case syspro.tm.lexer.Symbol.LESS_THAN_EQUALS => ???
    case syspro.tm.lexer.Symbol.GREATER_THAN => ???
    case syspro.tm.lexer.Symbol.GREATER_THAN_EQUALS => ???
    case syspro.tm.lexer.Symbol.LESS_THAN_LESS_THAN => ???
    case syspro.tm.lexer.Symbol.GREATER_THAN_GREATER_THAN => ???
    case syspro.tm.lexer.Symbol.OPEN_BRACKET => ???
    case syspro.tm.lexer.Symbol.CLOSE_BRACKET => ???
    case syspro.tm.lexer.Symbol.OPEN_PAREN => ???
    case syspro.tm.lexer.Symbol.CLOSE_PAREN => ???
    case syspro.tm.lexer.Symbol.EQUALS => ???
    case syspro.tm.lexer.Symbol.EQUALS_EQUALS => ???
    case syspro.tm.lexer.Symbol.EXCLAMATION_EQUALS => ???
    case syspro.tm.lexer.Symbol.QUESTION => ???
    case syspro.tm.lexer.Symbol.BOUND => ???
  }
}

