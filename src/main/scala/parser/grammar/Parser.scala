package org.syspro.spc
package parser.grammar

import scala.Conversion
import org.syspro.spc.parser.parsing_tree.*
import org.syspro.spc.parser.token.SyntaxKindConverter
import syspro.tm.lexer.{BadToken, Token}
import syspro.tm.parser.SyntaxKind

import scala.annotation.tailrec
sealed trait Result[+A]
case class Success[+A](result: A, remain_input: List[Token]) extends Result[A]
case class Failure(msg: Predef.String) extends Result[Nothing]

/*
enum Result[+A] {
  case Success(result: A, remain_input: List[Token])
  case Failure(msg: Predef.String)
  // TODO for better ERROR handling maybe I should add Fatal here
}
 */

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
 * Doesn't support packard parsing, or other methods of eliminating left recursion,
 * so you need to eliminate left recursion in your grammar to use this lib
 * @tparam A Return type of parser
 */
trait Parser[+A] extends (List[Token] => Result[A]) with Combinators {

  def apply(input: List[Token]): Result[A]

  /**
   * Simple andThen combinator
   *
   * If parser this succeeds, then runs parser b on the remaining input
   * @param b parser which runs after Success of 'this' parser
   * @tparam U thing, that parser p parse
   * @return parser which parse (A, U)
   */
  def ~[U](b: => Parser[U]): Parser[(A, U)] = {
    val a: Parser[A] = this // just for beauty of code

    (input: List[Token]) => {
      val res_a = a(input)

      res_a match {
        case Failure(msg) => Failure(msg)
        case Success(ir_a, remain_input_a) => {
          println(b)
          val res_b = b(remain_input_a)
          println(remain_input_a)
          res_b match {
            case Failure(msg) => Failure(msg)
            case Success(ir_b, remain_input_b) => Success((ir_a, ir_b), remain_input_b)
          }
        }
      }
    }
  }

  // TODO: doc
  def ~>[U](b: => Parser[U]): Parser[U] = {
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

  def <~[U](b: => Parser[U]): Parser[A] = {
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
  def <|> [U >: A](b: => Parser[U]): Parser[U] = {
    val a: Parser[A] = this // just for code beauty

    (input: List[Token]) => {
      val res_a = a(input)

      res_a match
        case Success(_, _) => res_a
        case Failure(msg) => b(input)
    }
  }

  def map[U](f: A => U): Parser[U] = {
    val parser: Parser[A] = this // just for code beauty

    (input: List[Token]) => {
      val res = parser(input)

      res match
        case Success(result, remain_input) => Success(f(result), remain_input)
        case Failure(msg) => Failure(msg)
    }
  }

  /*
  problem I had parser of lists of pairs
  I need list of parsers of pairs
  Parser
  def ^^^[A](): Parser[List[A]] => List[Parser[A]]
   */


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


  // TODO: make nice error handling can I?
}

trait Combinators {
  /**
   * Repeat this parser until it succeeds, or until input is not empty
   * @return
   */
  def repeat[A](p: => Parser[A]): Parser[List[A]] = {
    def repeatOnce(p: => Parser[A]): Parser[(A, A)] = p ~ p

    ???
  }
}

object Parser {
  def consume[A](input: List[Token])(consumer: Token => Result[A]): Result[A] = {
    input match
      case token :: tail => consumer(token)
      case _ => Failure("Eof")
  }
}


// TODO: add doc
object BasicLeafParser {
  /**
   * @param toMatch syntax of consumed token
   * @return parser, that consumes one input terminal token, return Leaf node for this token
   */
  def apply(toMatch: Syntax): Parser[Leaf] = {

    (input: List[Token]) => {
      Parser.consume(input) { (token: Token) =>
        val syntax: Syntax = SyntaxKindConverter(token)

        if (toMatch == syntax) Success(toMatch.of(token), input.tail) else Failure(s"Can't parse Token $token, expected syntax kind $toMatch, found $syntax")
      }



      /*
      input match
        case token :: tail =>
          val syntax: Syntax = SyntaxKindConverter(token)

          if (toMatch == syntax) Success(toMatch.of(token), input.tail) else Failure(s"Can't parse Token $token, expected syntax kind $toMatch, found $syntax")

        case _ => Failure("Input is empty")

       */
    }
  }

  given Conversion[Predef.String, Parser[Leaf]] with
    def apply(symbol: Predef.String): Parser[Leaf] = BasicLeafParser(Symbol(symbol))

  given Conversion[Syntax, Parser[Leaf]] with
    def apply(syntax: Syntax): Parser[Leaf] = BasicLeafParser(syntax)

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