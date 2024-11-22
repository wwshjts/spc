package org.syspro.spc
package parser.grammar

import parser.parsing_tree.*

/**
 * Grammar of SysPro lang, written in my DSL of parser combinators
 */
object Grammar {

  import BasicLeafParser.{given_Conversion_String_Parser, given_Conversion_Syntax_Parser}

  // **** Priority 0 ****
  // primary expression
  def integer: Parser[IntegerLiteral]   = INTEGER ^^ (i => IntegerLiteral(i))
  def string: Parser[StringLiteral]     = STRING ^^ (s => StringLiteral(s))

  def literal_expr: Parser[Unary] = integer <|> string

  // **** Priority 1 ****
  // Unary expression
  def unary: Parser[Unary] = (negate <|> u_plus <|> bitwiseNot) <|> literal_expr

  def negate: Parser[Negate]            = ("-" ~ unary) ^^ (p => Negate(p._2))
  def u_plus: Parser[UPlus]             = ("+" ~ unary) ^^ (p => UPlus(p._2))
  def bitwiseNot: Parser[BitwiseNot]    = ("~" ~ unary) ^^ (p => BitwiseNot(p._2))

  // Binary expressions
  // **** Priority 2 ****
}
