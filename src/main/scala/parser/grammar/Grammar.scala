package org.syspro.spc
package parser.grammar

import parser.parsing_tree.*

/**
 * Grammar of SysPro lang, written in my DSL of parser combinators
 */
object Grammar {

  import BasicLeafParser.{given_Conversion_String_Parser, given_Conversion_DSLEntity_Parser}


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

  // def term: Parser[Binary] = (unary ~ ("*" <|> "/")) <|> unary <|> term
  def factor = (unary ~ Combinators.repeatAtLeastOnce(("*" <|> "/") ~ unary)) ^^ (
    parsed => {
      val u = parsed._1
      val list = parsed._2
      println(list)

      list match
        case head :: tail => {
          val first = head._1 match {
            case ASTERISK(_) => MULTIPLY(u, head._1, head._2)
            case SLASH(_) => DIV(u, head._1, head._2)
          }

          tail.foldLeft(first)((left, op) => {
            op._1 match
              case ASTERISK(_) => MULTIPLY(left, op._1, op._2)
              case SLASH(_) => DIV(left, op._1, op._2)

          })
        }
    }
    )
}
