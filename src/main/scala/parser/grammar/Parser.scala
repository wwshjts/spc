package org.syspro.spc
package parser.grammar

import org.syspro.spc.parser.grammar.Result.{Failure, Success}
import org.syspro.spc.parser.parsing_tree.{Leaf, PLUS, ParsingTree, Symbol}
import syspro.tm.lexer.{BadToken, Token}
import syspro.tm.parser.SyntaxKind

enum Result[+A] {
  case Success(result: A, remain_input: List[Token])
  case Failure(string: Predef.String)
}

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
trait Parser[+A] extends (List[Token] => Result[A]) {

  def apply(input: List[Token]): Result[A]

  // TODO: make error handling
}

trait ParserFactory[A] extends (A => Parser[A]) {
  def apply(toMatch: A): Parser[A]
}

object SymbolParser extends ParserFactory[Symbol] {
  override def apply(toMatch: Symbol): Parser[Symbol] = {
    (input: List[Token]) => {
      val tkn = input.head
      println(tkn)

      if (Symbol.of.isDefinedAt(tkn)) {
        if (Symbol.of(tkn) == toMatch) Success(Symbol.of(toMatch, tkn), input.tail) else Failure(s"Expected $toMatch, found ${Symbol.of(tkn)}")

      } else Result.Failure("Failed to parse symbol")
    }
  }
}

val plus = SymbolParser(PLUS)