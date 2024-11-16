package org.syspro.spc
package parser.grammar

import org.syspro.spc.parser.parsing_tree.{Leaf, ParsingTree}
import syspro.tm.lexer.Token
import syspro.tm.parser.SyntaxKind

/**
 * A parser for things
 *
 * Is a function from list of syspro.tm.lexer.Token
 *
 * To lists of pairs
 *
 * of things and syspro.tm.lexer.Token
 * @tparam A
 */
trait Parser[A] extends (List[Token] => Option[(A, List[Token])]){

  def apply(input: List[Token]): Option[(A, List[Token])]

  // TODO: make error handling
}

trait ParserFactory[A] {
  def apply(syntaxKind: SyntaxKind): Parser[A]
}

object ParseTerminal extends ParserFactory[ParsingTree] {
  def apply(syntaxKind: SyntaxKind): Parser[ParsingTree] = {
    (input: List[Token]) => {
      val tkn: Token = input.head
      println(input)

      println(tkn)

      if (syntaxKind == tkn.toSyntaxKind) Some((Leaf(syntaxKind, tkn), input.tail)) else None
    }
  }
}

val integer   = ParseTerminal(SyntaxKind.INTEGER)
val boolean   = ParseTerminal(SyntaxKind.BOOLEAN)
val string    = ParseTerminal(SyntaxKind.STRING)
val rune      = ParseTerminal(SyntaxKind.RUNE)

val ident     = ParseTerminal(SyntaxKind.INDENT)
val dedent    = ParseTerminal(SyntaxKind.DEDENT)

val bad       = ParseTerminal(SyntaxKind.BAD)
