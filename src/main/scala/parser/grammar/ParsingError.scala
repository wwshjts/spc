package org.syspro.spc
package parser.grammar

import org.syspro.spc.parser.parsing_tree.{DSLEntity, Terminal}
import syspro.tm.lexer.Token

enum ParsingError {
  case MissMatchToken(curr_token: Token, expected: DSLEntity, found: DSLEntity)
  case NothingToConsume(expected: DSLEntity)

  case AtomErr

  case PrimaryErr

  override def toString: String = this match
    case ParsingError.MissMatchToken(curr_token, expected, found) =>
      s"Can't parse Token $curr_token, expected syntax kind $expected, found $found\n"

    case ParsingError.NothingToConsume(expected: DSLEntity) =>
      s"Eof found, but expected $expected"

    case ParsingError.AtomErr =>
      s"This token should be atomic"

    case ParsingError.PrimaryErr =>
      s"Some error"
}