package org.syspro.spc
package parser.grammar

import org.syspro.spc.parser.grammar.Result.{Failure, Success}
import org.syspro.spc.parser.parsing_tree.{BuiltInType, Leaf, PLUS, ParsingTree, Symbol, Syntax}
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
   * @param b
   * @tparam U
   * @return returned parser
   */
  def ~>[U](b: Parser[U]): Parser[(A, U)] = {
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


val plus = BasicSyntaxParser(PLUS)

/*
primitive_expr = INTEGER_EXPR | ... | RUNE_EXPR

ADD_EXPR = (EXPR PLUS) EXPR : Parser[ADD_EXPR]
            Parser[(EXPR, PLUS)]
              Parser[((EXPR, PLUS), EXPR)] :>> Parser[ADD_EXPR]

INTEGER_EXPR = INTEGER

E = E + E | E * E | T
M = E * E
T = INTEGER

 */