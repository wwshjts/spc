package org.syspro.spc
package parser.grammar

import org.syspro.spc.parser.grammar.Result.{Failure, Success}
import org.syspro.spc.parser.parsing_tree.{BuiltInType, INTEGER, Leaf, PLUS, ParsingTree, Symbol, Syntax}
import org.syspro.spc.parser.token.SyntaxKindConverter
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

  /**
   * Simple andThen combinator
   *
   * If parser this succeeds, then runs parser b on the remaining input
   * @param b parser which runs after Success of 'this' parser
   * @tparam U thing, that parser p parse
   * @return parser which parse (A, U)
   */
  def ~[U](b: Parser[U]): Parser[(A, U)] = {
    val a: Parser[A] = this // just for beauty of code

    (input: List[Token]) => {
      val res_a = a(input)

      res_a match {
        case Failure(msg) => Failure(msg)
        case Success(ir_a, remain_input_a) => {
          val res_b = b(remain_input_a)

          res_b match {
            case Failure(msg) => Failure(msg)
            case Success(ir_b, remain_input_b) => Success((ir_a, ir_b), remain_input_b)
          }
        }
      }
    }
  }


  /**
   * orElse parser combinator
   * @tparam U should be super type of this parser type parameter
   * @return result of first parser if it succeeds, else if b succeeds returns result of b,
   *         otherwise fails
   */
  def <|> [U >: A](b: Parser[U]): Parser[U] = {
    val a: Parser[A] = this // just for code beauty

    (input: List[Token]) => {
      val res_a = a(input)

      res_a match
        case Result.Success(_, _) => res_a
        case Result.Failure(msg) => b(input)
    }
  }


  // TODO: make nice error handling
}

// TODO: add conversion from strings
object BasicSyntaxParser {
  def apply(toMatch: Syntax): Parser[Leaf] = {
    (input: List[Token]) => {
      val tkn = input.head
      val syntax: Syntax = SyntaxKindConverter(tkn)

      if (toMatch == syntax) Success(toMatch.of(tkn), input.tail) else Failure(s"Can't parse Token $tkn, expected syntax kind $toMatch, found $syntax")
    }
  }
}

/*
primitive_expr = INTEGER_EXPR | ... | RUNE_EXPR

ADD_EXPR = (EXPR PLUS) EXPR : Parser[ADD_EXPR]
            Parser[(EXPR, PLUS)]
              Parser[((EXPR, PLUS), EXPR)] :>> Parser[ADD_EXPR]

INTEGER_EXPR = INTEGER

E = binOP
M = E * E
T = INTEGER

 */