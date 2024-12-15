package org.syspro.spc
package parser.grammar

import org.syspro.spc.parser.grammar.ParsingError.{MissMatchToken, NothingToConsume}
import org.syspro.spc.parser.parsing_tree.DSLEntity

import scala.Conversion
import org.syspro.spc.parser.parsing_tree.*
import org.syspro.spc.parser.token.SyntaxKindConverter
import syspro.tm.lexer.Token

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

sealed trait Result[+A] {
  def map[U](f: A => U): Result[U]
  def get: A
  def remain: List[Token]
}

case class Success[+A](result: A, remain_input: List[Token]) extends Result[A] {
  override def map[U](f: A => U): Result[U] = Success(f(result), remain_input)

  override def get: A = result
  override def remain: List[Token] = remain_input
}

// TODO: refactor Failures cases
case class Failure(error: ParsingError, remain_input: List[Token]) extends Result[Nothing] {
  override def map[U](f: Nothing => U): Result[U] = Failure(error, remain_input)

  override def get: Nothing = ???

  override def remain: List[Token] = remain_input

  override def toString: String = remain_input match
    case head :: tail =>
      val token = remain_input.head
      s"Error on token: $token\n pos: ${(token.start, token.end)}\n" + error
    case _ => error.toString
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
 * Doesn't support packard parsing, or other methods of eliminating left recursion,
 * so you need to eliminate left recursion in your grammar to use this lib
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
  def ~[U](b: => Parser[U]): Parser[(A, U)] = {
    val a: Parser[A] = this // just for beauty of code

    (input: List[Token]) => {
      val res_a = a(input)

      res_a match {
        case Failure(msg, remain_input) => Failure(msg, remain_input)
        case Success(ir_a, remain_input_a) => {
          val res_b = b(remain_input_a)
          res_b match {
            case Failure(msg, remain_input) => Failure(msg, remain_input)
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
        case Failure(msg, remain_input) => Failure(msg, remain_input)
        case Success(ir_a, remain_input_a) => {
          val res_b = b(remain_input_a)

          res_b match {
            case Failure(msg, remain_input) => Failure(msg, remain_input)
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
        case Failure(msg, remain_input) => Failure(msg, remain_input)
        case Success(ir_a, remain_input_a) => {
          val res_b = b(remain_input_a)

          res_b match {
            case Failure(msg, remain_input) => Failure(msg, remain_input)
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
        case Failure(_, _) => b(input)
    }
  }

  def map[U](f: A => U): Parser[U] = {
    val parser: Parser[A] = this // just for code beauty

    (input: List[Token]) => {
      val res = parser(input)

      res match
        case Success(result, remain_input) => Success(f(result), remain_input)
        case Failure(msg, remain_input) => Failure(msg, remain_input)
    }
  }
  
  def flatMap[U](f: A => Parser[U]): Parser[U] = {
    ???
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

  /**
   * Marks parser whits specific error code
   *
   * Really useful when combining parsers
   * @param error Code of error
   * @return Parser, that marked with error code
   */
  def mark(error: ParsingError): Parser[A] = {
    val p: Parser[A] = this

    (input: List[Token]) => {
      p(input) match
        case s @ Success(result, remain_input) => s
        case Failure(old_error, remain_input) => Failure(error, remain_input)
    }
  }

  /**
   * Marks parser whits specific error code
   *
   * Really useful when combining parsers
   *
   * @param error Code of error
   * @return Parser, that marked with error code
   */
  def |?|(error: ParsingError): Parser[A] = mark(error)
}

object Combinators {
  def **[A](p: => Parser[A]): Parser[(A, List[A])] = repeatAtLeastOnce(p)

  def |**[A](p: => Parser[A]): Parser[List[A]] = repeatAtLeastOnce(p) ^^ ({
    case (head, tail) => head :: tail
  }
    )

  def *?[A](p: => Parser[A]): Parser[List[A]] = repeat(p)

  def repeat[A](p: => Parser[A]): Parser[List[A]] = {
    def applyp(input: List[Token]): Result[List[A]] = {
      |**(p)(input) match
        case Success(result, remain_input) => Success(result, remain_input)
        case Failure(msg, remain_input) => Success(List.empty, input)
    }
    applyp
  }

  def repeatAtLeastOnce[A](p: => Parser[A]): Parser[(A, List[A])] = repeatAtLeastOnce(p, p)

  def repeatAtLeastOnce[A](first: => Parser[A], p0: => Parser[A]): Parser[(A, List[A])] = { (in: List[Token]) =>
    lazy val p = p0
    val elems = new ListBuffer[A]

    def continue(in: List[Token]): Success[List[A]] = {
      val p0 = p
      @tailrec def applyp(in0: List[Token]): Success[List[A]] =
        if (in0.isEmpty) return Success(elems.toList, in0)
        p0(in0) match {
        case Success(result, remain_input) =>
          elems += result
          applyp(remain_input)

        case f: Failure => Success(elems.toList, in0)
      }

      applyp(in)
    }

    first(in) match {
      case Success(result, remain_input) =>
        continue(remain_input) match
          case Success(r1, remain_input) => Success((result, r1), remain_input)
      case f @ Failure(msg, remain_input) => f
    }
  }

  def ?[A](p: => Parser[A]): Parser[Option[A]] = has(p)

  /** Matches expression that p matches, or returns empty list */
  def has[A](p: => Parser[A]): Parser[Option[A]] = {
    (input: List[Token]) => {
      p(input) match
        case Success(result, remain_input) => Success(Some(result), remain_input)
        case Failure(msg, _) => Success(None, input)
    }
  }

}

// TODO: add doc
object BasicLeafParser {
  /**
   * @param toMatch syntax of consumed token
   * @return parser, that consumes one input terminal token, return Leaf node for this token
   */
  def apply(toMatch: DSLEntity): Parser[Terminal] = {
    def consume(input: List[Token]): Result[Terminal] =
      val token = input.head
      val syntax = SyntaxKindConverter(token)
      if (syntax == toMatch)
        Success(toMatch(token), input.tail)
      else
        Failure(MissMatchToken(token, toMatch, syntax), input)

    (input: List[Token]) =>
      if (input.nonEmpty)
        consume(input)
      else
        Failure(NothingToConsume(toMatch), input)
  }

  /** Parser, that always succeeds. Doesn't consume any tokens */
  def eps[A]: Parser[A] = {
    (input: List[Token]) => {
      Success(null.asInstanceOf[A], input)
    }
  }

  // Conversion from String, eah, type will be deduced to Terminal
  given Conversion[Predef.String, Parser[Terminal]] with
    def apply(symbol: Predef.String): Parser[Terminal] = BasicLeafParser(Symbol(symbol))

  given Conversion[DSLEntity, Parser[Terminal]] with
    def apply(entity: DSLEntity): Parser[Terminal] = BasicLeafParser(entity)

}
