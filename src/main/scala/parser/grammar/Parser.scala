package org.syspro.spc
package parser.grammar

import scala.Conversion
import org.syspro.spc.parser.grammar.Result.{Failure, Success}
import org.syspro.spc.parser.parsing_tree.*
import org.syspro.spc.parser.token.SyntaxKindConverter
import syspro.tm.lexer.{BadToken, Token}
import syspro.tm.parser.SyntaxKind

import scala.annotation.tailrec

enum Result[+A] {
  case Success(result: A, remain_input: List[Token])
  case Failure(msg: Predef.String)
  // TODO for better ERROR handling maybe I should add Fatal here
}

/**
 * A parser for things
 *
 * Is a function from list of syspro.tm.lexer.Token
 *
 * To lists of pairs
 *
 * of things and syspro.tm.lexer.Token
 *
 * Very simple combinator lib, literally dsl for recursive descending parsing
 *
 * Doesn't support packard parser, or other methods of eliminating left recursion,
 * so you need to eliminate left recursion
 * @tparam A Return type of parser
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

  // TODO: doc
  def ~>[U](b: Parser[U]): Parser[U] = {
    val a: Parser[A] = this // just for beauty of code

    (input: List[Token]) => {
      val res_a = a(input)

      res_a match {
        case Failure(msg) => Failure(msg)
        case Success(ir_a, remain_input_a) => {
          val res_b = b(remain_input_a)

          res_b match {
            case Failure(msg) => Failure(msg)
            case Success(ir_b, remain_input_b) => Success(ir_b, remain_input_b)
          }
        }
      }
    }
  }

  def <~[U](b: Parser[U]): Parser[A] = {
    val a: Parser[A] = this // just for beauty of code

    (input: List[Token]) => {
      val res_a = a(input)

      res_a match {
        case Failure(msg) => Failure(msg)
        case Success(ir_a, remain_input_a) => {
          val res_b = b(remain_input_a)

          res_b match {
            case Failure(msg) => Failure(msg)
            case Success(ir_b, remain_input_b) => Success(ir_a, remain_input_b)
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

  def map[U](f: A => U): Parser[U] = {
    val parser: Parser[A] = this // just for code beauty

    (input: List[Token]) => {
      val res = parser(input)

      res match
        case Result.Success(result, remain_input) => Result.Success(f(result), remain_input)
        case Result.Failure(msg) => Failure(msg)
    }
  }

  /**
   * Map parser combinator
   * @param A
   * @tparam U
   * @return
   */
  def ^^[U] (f: A => U): Parser[U] = map(f)

  // TODO: add do notation here
  // apply this parser to input while parser succeeds and input has tokens
  // А вот че за параметр A должен возвращать do Комбинатор???

  /**
   *
   * @return
   */
  def |*(): Parser[Seq[A]] = {
    val p: Parser[A] = this // just for code beauty

    (input: List[Token]) => {
      val res = p(input)
      res match
        case Result.Success(result, remain_input) => ???
        case Result.Failure(msg) => Failure(msg)
    }
  }

  // TODO: make nice error handling can I?
}


// TODO: add doc
object BasicSyntaxParser {
  def apply[A <: Syntax, B <: Leaf](toMatch: A): Parser[B] = {

    (input: List[Token]) => {
      val tkn = input.head
      val syntax: Syntax = SyntaxKindConverter(tkn)

      if (toMatch == syntax) Success(toMatch.of(tkn).asInstanceOf[B], input.tail) else Failure(s"Can't parse Token $tkn, expected syntax kind $toMatch, found $syntax")
    }

  }

  given Conversion[Predef.String, Parser[Symbol]] with
    def apply(symbol: Predef.String): Parser[Symbol] = {
      BasicSyntaxParser(Symbol(symbol))
    }

}


// something like this, or add conversion from lists
//import BasicSyntaxParser.given_Conversion_String_Parser

/*
primitive_expr = INTEGER_EXPR | ... | RUNE_EXPR

ADD_EXPR = (EXPR PLUS) EXPR : Parser[ADD_EXPR]
            Parser[(EXPR, PLUS)]
              Parser[((EXPR, PLUS), EXPR)] :>> Parser[ADD_EXPR]

plus = "+"
lots_plus = |* plus => ((( .. (Plus, Plus)) ... )

val unary: Parser[Unary] = ( ("!" <|> "-") ~ unary ) <|> primary

 */