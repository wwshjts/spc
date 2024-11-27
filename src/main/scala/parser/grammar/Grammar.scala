package org.syspro.spc
package parser.grammar

import parser.parsing_tree.*

/**
 * Grammar of SysPro lang, written in my DSL of parser combinators
 */
object Grammar {

  import BasicLeafParser.{given_Conversion_String_Parser, given_Conversion_DSLEntity_Parser}
  import Combinators.*


  // **** Priority 0 ****
  // primary expression
  def integer: Parser[IntegerLiteral]   = INTEGER ^^ (i => IntegerLiteral(i))
  def string: Parser[StringLiteral]     = STRING ^^ (s => StringLiteral(s))
  def bool: Parser[BooleanLiteral]      = BOOLEAN ^^ (b => BooleanLiteral(b))
  def rune: Parser[RuneLiteral]         = RUNE ^^ (r => RuneLiteral(r))

  def literal_expr: Parser[LiteralExpr] = integer <|> string <|> bool <|> rune

  // **** Priority 1 ****
  // Unary expression
  def unary: Parser[Expression] = (negate <|> u_plus <|> bitwiseNot) <|> literal_expr

  def negate: Parser[Negate]            = ("-" ~ unary) ^^ (p => Negate(p._1, p._2))
  def u_plus: Parser[UPlus]             = ("+" ~ unary) ^^ (p => UPlus(p._1, p._2))
  def bitwiseNot: Parser[BitwiseNot]    = ("~" ~ unary) ^^ (p => BitwiseNot(p._1, p._2))

  // Binary expressions
  // **** Priority 2 ****

  // this thing parses (unary, List[(Terminal, Unary)) -> Binary
  // firstly I need to get first binary expression, then try to fold tail
  // (unary, List[(Terminal, Unary)) -> (unary, (Terminal, unary, List[(terminal, unary))) -> (binary, List(token
  def factor = (unary ~ **(("*" <|> "/") ~ unary)) ^^ (
    parsed => {
      val (left, ((op, right), tail)) = parsed

      val first = op match
        case ASTERISK(_) => MULTIPLY(left, op, right)
        case SLASH(_) => DIV(left, op, right)

      tail.foldLeft(first)((left, term) =>
        val (op, right) = term
        op match
          case  ASTERISK(_) => MULTIPLY(left, op, right)
          case  SLASH(_) => DIV(left, op, right)
      )
    })
}
