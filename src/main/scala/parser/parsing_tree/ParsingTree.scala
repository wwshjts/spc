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

// Indentation and other syntax
case class INDENT(tkn: Token)       extends Leaf(tkn)
case class DEDENT(tkn: Token)       extends Leaf(tkn)

case class IDENTIFIER(tkn: Token)   extends Leaf(tkn)
case class RUNE(tkn: Token)         extends Leaf(tkn)

// Built-in types
case class BOOLEAN(tkn: Token)      extends Leaf(tkn)
case class INTEGER(tkn: Token)      extends Leaf(tkn)
case class STRING(tkn: Token)       extends Leaf(tkn)

// Symbols
case class DOT(tkn: Token)          extends Leaf(tkn)
case class COLON(tkn: Token)        extends Leaf(tkn)
case class COMMA(tkn: Token)        extends Leaf(tkn)
case class PLUS(tkn: Token)         extends Leaf(tkn)
case class MINUS(tkn: Token)        extends Leaf(tkn)
case class ASTERISK(tkn: Token)     extends Leaf(tkn)
case class SLASH(tkn: Token)        extends Leaf(tkn)
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


// Syntax
sealed trait Syntax {
  // TODO: def kind(): AnySyntaxKind

  // TODO
  def of(token: Token): Leaf = {
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
// TODO: add all Symbols

sealed trait BuiltInType  extends Syntax
case object INTEGER       extends BuiltInType
case object BOOLEAN       extends BuiltInType
case object STRING        extends BuiltInType


/*
object Symbol {
  /**
   * Construct Symbol Leaf of ParsingTree IR
   * @param token token from Leaf constructed
   * @return case object for corresponding symbol
   */
  val of: PartialFunction[Token, Symbol] = (token: Token) => token match
    case _ if token.toSyntaxKind.isInstanceOf[syspro.tm.lexer.Symbol] => {
      val symbol = token.toSyntaxKind.asInstanceOf[syspro.tm.lexer.Symbol]
      symbol match {
        case syspro.tm.lexer.Symbol.DOT => DOT
        case syspro.tm.lexer.Symbol.COLON => COLON
        case syspro.tm.lexer.Symbol.COMMA => COMMA
        case syspro.tm.lexer.Symbol.PLUS => PLUS
        case syspro.tm.lexer.Symbol.MINUS => MINUS
        case syspro.tm.lexer.Symbol.ASTERISK => ASTERISK
        case syspro.tm.lexer.Symbol.SLASH => SLASH
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

  def of(symbol: Symbol, token: Token): Symbol = symbol match
    case DOT => DOT(token)
    case COLON => COLON(token)
    case COMMA => COMMA(token)
    case PLUS => PLUS(token)
    case MINUS => MINUS(token)
    case ASTERISK => ASTERISK(token)
    case SLASH => SLASH(token)
}
 */
