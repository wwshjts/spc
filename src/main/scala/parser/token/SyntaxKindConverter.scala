package org.syspro.spc
package parser.token

import org.syspro.spc.parser.parsing_tree.{AMPERSAND_AMPERSAND, BAD, DSLEntity, ParsingTree, Symbol, Terminal}
import org.syspro.spc.parser.parsing_tree
import syspro.tm.lexer
import syspro.tm.lexer.{IdentifierToken, Keyword, KeywordToken, Token}
import syspro.tm.parser.SyntaxKind

object SyntaxKindConverter {
  def apply(token: Token): DSLEntity = {
    // I have one 'soft' keyword here, in DSL is really important t
    if (token.toSyntaxKind == SyntaxKind.IDENTIFIER && token.asInstanceOf[IdentifierToken].contextualKeyword != null)
      parsing_tree.NULL
    else
      token.toSyntaxKind match
        case kw: Keyword => keyword(kw)
        case sym: lexer.Symbol => symbol(sym)
        case knd: SyntaxKind => kind(knd)
  }

  def keyword(kw: Keyword): DSLEntity = {
    kw match
      case Keyword.THIS => parsing_tree.THIS
      case Keyword.SUPER => parsing_tree.SUPER
      case Keyword.IS => parsing_tree.IS
      case Keyword.IF => parsing_tree.IF
      case Keyword.ELSE => parsing_tree.ELSE
      case Keyword.FOR => parsing_tree.FOR
      case Keyword.IN => parsing_tree.IN
      case Keyword.WHILE => parsing_tree.WHILE
      case Keyword.DEF => ???
      case Keyword.VAR => ???
      case Keyword.VAL => ???
      case Keyword.RETURN => parsing_tree.RETURN
      case Keyword.BREAK => parsing_tree.BREAK
      case Keyword.CONTINUE => parsing_tree.CONTINUE
      case Keyword.ABSTRACT => ???
      case Keyword.VIRTUAL => ???
      case Keyword.OVERRIDE => ???
      case Keyword.NATIVE => ???
      case Keyword.CLASS => ???
      case Keyword.OBJECT => ???
      case Keyword.INTERFACE => ???
      case Keyword.NULL => parsing_tree.NULL
  }

  def kind(k: SyntaxKind): DSLEntity = {
    k match
      case SyntaxKind.BAD => parsing_tree.BAD
      case SyntaxKind.INDENT => parsing_tree.INDENT
      case SyntaxKind.DEDENT => parsing_tree.DEDENT
      case SyntaxKind.IDENTIFIER => parsing_tree.IDENTIFIER
      case SyntaxKind.BOOLEAN => parsing_tree.BOOLEAN
      case SyntaxKind.INTEGER => parsing_tree.INTEGER
      case SyntaxKind.RUNE => parsing_tree.RUNE
      case SyntaxKind.STRING => parsing_tree.STRING
      case SyntaxKind.SOURCE_TEXT => ???
      case SyntaxKind.TYPE_BOUND => ???
      case SyntaxKind.LIST => ???
      case SyntaxKind.SEPARATED_LIST => ???
      case SyntaxKind.TYPE_DEFINITION => ???
      case SyntaxKind.FUNCTION_DEFINITION => ???
      case SyntaxKind.VARIABLE_DEFINITION => ???
      case SyntaxKind.TYPE_PARAMETER_DEFINITION => ???
      case SyntaxKind.PARAMETER_DEFINITION => ???
      case SyntaxKind.VARIABLE_DEFINITION_STATEMENT => ???
      case SyntaxKind.ASSIGNMENT_STATEMENT => ???
      case SyntaxKind.EXPRESSION_STATEMENT => ???
      case SyntaxKind.RETURN_STATEMENT => ???
      case SyntaxKind.BREAK_STATEMENT => ???
      case SyntaxKind.CONTINUE_STATEMENT => ???
      case SyntaxKind.IF_STATEMENT => ???
      case SyntaxKind.WHILE_STATEMENT => ???
      case SyntaxKind.FOR_STATEMENT => ???
      case SyntaxKind.LOGICAL_AND_EXPRESSION => ???
      case SyntaxKind.LOGICAL_OR_EXPRESSION => ???
      case SyntaxKind.LOGICAL_NOT_EXPRESSION => ???
      case SyntaxKind.EQUALS_EXPRESSION => ???
      case SyntaxKind.NOT_EQUALS_EXPRESSION => ???
      case SyntaxKind.LESS_THAN_EXPRESSION => ???
      case SyntaxKind.LESS_THAN_OR_EQUAL_EXPRESSION => ???
      case SyntaxKind.GREATER_THAN_EXPRESSION => ???
      case SyntaxKind.GREATER_THAN_OR_EQUAL_EXPRESSION => ???
      case SyntaxKind.IS_EXPRESSION => ???
      case SyntaxKind.BITWISE_AND_EXPRESSION => ???
      case SyntaxKind.BITWISE_OR_EXPRESSION => ???
      case SyntaxKind.BITWISE_EXCLUSIVE_OR_EXPRESSION => ???
      case SyntaxKind.BITWISE_LEFT_SHIFT_EXPRESSION => ???
      case SyntaxKind.BITWISE_RIGHT_SHIFT_EXPRESSION => ???
      case SyntaxKind.ADD_EXPRESSION => ???
      case SyntaxKind.SUBTRACT_EXPRESSION => ???
      case SyntaxKind.MULTIPLY_EXPRESSION => ???
      case SyntaxKind.DIVIDE_EXPRESSION => ???
      case SyntaxKind.MODULO_EXPRESSION => ???
      case SyntaxKind.UNARY_PLUS_EXPRESSION => ???
      case SyntaxKind.UNARY_MINUS_EXPRESSION => ???
      case SyntaxKind.BITWISE_NOT_EXPRESSION => ???
      case SyntaxKind.MEMBER_ACCESS_EXPRESSION => ???
      case SyntaxKind.INVOCATION_EXPRESSION => ???
      case SyntaxKind.INDEX_EXPRESSION => ???
      case SyntaxKind.THIS_EXPRESSION => ???
      case SyntaxKind.SUPER_EXPRESSION => ???
      case SyntaxKind.NULL_LITERAL_EXPRESSION => ???
      case SyntaxKind.TRUE_LITERAL_EXPRESSION => ???
      case SyntaxKind.FALSE_LITERAL_EXPRESSION => ???
      case SyntaxKind.STRING_LITERAL_EXPRESSION => ???
      case SyntaxKind.RUNE_LITERAL_EXPRESSION => ???
      case SyntaxKind.INTEGER_LITERAL_EXPRESSION => ???
      case SyntaxKind.PARENTHESIZED_EXPRESSION => ???
      case SyntaxKind.IDENTIFIER_NAME_EXPRESSION => ???
      case SyntaxKind.OPTION_NAME_EXPRESSION => ???
      case SyntaxKind.GENERIC_NAME_EXPRESSION => ???

  }

  def symbol(s: lexer.Symbol): Symbol = s match {
    case lexer.Symbol.COLON => parsing_tree.COLON
    case lexer.Symbol.COMMA => parsing_tree.COMMA
    case lexer.Symbol.PLUS => parsing_tree.PLUS
    case lexer.Symbol.MINUS => parsing_tree.MINUS
    case lexer.Symbol.ASTERISK => parsing_tree.ASTERISK
    case lexer.Symbol.SLASH => parsing_tree.SLASH
    case lexer.Symbol.PERCENT => parsing_tree.PERCENT
    case lexer.Symbol.EXCLAMATION => parsing_tree.EXCLAMATION
    case lexer.Symbol.TILDE => parsing_tree.TILDE
    case lexer.Symbol.AMPERSAND => parsing_tree.AMPERSAND
    case lexer.Symbol.BAR => parsing_tree.BAR
    case lexer.Symbol.AMPERSAND_AMPERSAND => parsing_tree.AMPERSAND_AMPERSAND
    case lexer.Symbol.BAR_BAR => parsing_tree.BAR_BAR
    case lexer.Symbol.CARET => parsing_tree.CARET
    case lexer.Symbol.LESS_THAN => parsing_tree.LEFT
    case lexer.Symbol.LESS_THAN_EQUALS => parsing_tree.LEFT_EQ
    case lexer.Symbol.GREATER_THAN => parsing_tree.RIGHT
    case lexer.Symbol.GREATER_THAN_EQUALS => parsing_tree.RIGHT_EQ
    case lexer.Symbol.LESS_THAN_LESS_THAN => parsing_tree.LEFT_LEFT
    case lexer.Symbol.GREATER_THAN_GREATER_THAN => parsing_tree.RIGHT_RIGHT
    case lexer.Symbol.OPEN_BRACKET => parsing_tree.OPEN_BRACKET
    case lexer.Symbol.CLOSE_BRACKET => parsing_tree.CLOSE_BRACKET
    case lexer.Symbol.OPEN_PAREN => parsing_tree.OPEN_PAREN
    case lexer.Symbol.CLOSE_PAREN => parsing_tree.CLOSE_PAREN
    case lexer.Symbol.EQUALS => parsing_tree.EQ
    case lexer.Symbol.EQUALS_EQUALS => parsing_tree.EQ_EQ
    case lexer.Symbol.EXCLAMATION_EQUALS => parsing_tree.NEQ
    case lexer.Symbol.QUESTION => parsing_tree.QUESTION
    case lexer.Symbol.BOUND => ???
    case lexer.Symbol.DOT => parsing_tree.DOT
  }
}
