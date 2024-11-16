package org.syspro.spc
package parser.grammar

import org.syspro.spc.parser.parsing_tree.{Leaf, ParsingTree}
import syspro.tm.lexer.Token
import syspro.tm.parser.{AnySyntaxKind, SyntaxKind}

sealed trait Result {}
case class Success(parsingTree: ParsingTree, remaining_input: List[Token]) extends Result
case class Failure() extends Result


/**
 * A parser for things
 *
 * Is a function from list of syspro.tm.lexer.Token
 *
 * To lists of pairs
 *
 * of things and syspro.tm.lexer.Token
 */
trait Parser extends (List[Token] => Result) {

  def apply(input: List[Token]): Result

  // TODO: make error handling
}

object ParserCombinators {
  extension (input: List[Token])
    def >>(parser: Parser): Result = parser(input)

  extension (a: Parser)
    def ~>(b: Parser): Parser = {
      (input: List[Token]) => {
        val res_a = a(input)

        res_a match {
          case Failure() => Failure()
          case Success(tree, remaining_input) => b(remaining_input)
        }
      }
    }
}


trait ParserFactory {
  def apply(syntaxKind: AnySyntaxKind): Parser
}

object ParseTerminal extends ParserFactory {
  def apply(syntaxKind: AnySyntaxKind): Parser = {
    (input: List[Token]) => {
      val tkn: Token = input.head
      println(input)

      println(tkn)

      if (syntaxKind == tkn.toSyntaxKind) Success(Leaf(syntaxKind, tkn), input.tail) else Failure()
    }
  }
}

object ParseOp extends ParserFactory {
  override def apply(syntaxKind: AnySyntaxKind): Parser = {
    (input: List[Token]) => {
      val tkn: Token = input.head
      println(input)

      println(tkn)

      if (syntaxKind == tkn.toSyntaxKind) Success(Leaf(syntaxKind, tkn), input.tail) else Failure()
  }
}


val integer   = ParseTerminal(SyntaxKind.INTEGER)
val boolean   = ParseTerminal(SyntaxKind.BOOLEAN)
val string    = ParseTerminal(SyntaxKind.STRING)
val rune      = ParseTerminal(SyntaxKind.RUNE)

val ident     = ParseTerminal(SyntaxKind.INDENT)
val dedent    = ParseTerminal(SyntaxKind.DEDENT)

val bad       = ParseTerminal(SyntaxKind.BAD)

val add       = ParseOp(syspro.tm.lexer.Symbol.PLUS)

/*
val expr = integer | boolean | ... | bad
val add_expr =  (expr ~> add) <~ expr
input = 12 + 13
Branch( Plus
Left(12), Right(13) )
parser
 */
