package org.syspro.spc
package parser.grammar

import parser.parsing_tree.*

import org.syspro.spc.parser.parsing_tree

/**
 * Grammar of SysPro lang, written in my DSL of parser combinators
 */
object Grammar {

  import BasicLeafParser.{given_Conversion_String_Parser, given_Conversion_DSLEntity_Parser}
  import Combinators.*


  // **** Priority -1 ****
  // atoms
  def integer: Parser[IntegerLiteral]   = INTEGER ^^ IntegerLiteral
  def string: Parser[StringLiteral]     = STRING ^^ StringLiteral
  def bool: Parser[BooleanLiteral]      = BOOLEAN ^^ BooleanLiteral
  def rune: Parser[RuneLiteral]         = RUNE ^^ RuneLiteral
  def null_lit: Parser[NullLiteral]     = NULL ^^ NullLiteral
  def this_expr: Parser[ThisExpr]       = THIS ^^ ThisExpr
  def super_expr: Parser[SuperExpr]     = SUPER ^^ SuperExpr
  def identif: Parser[IdentifierName]   = IDENTIFIER ^^ IdentifierName

  def atom: Parser[Atom] = integer <|> string <|> bool <|> rune <|> this_expr
                                            <|> super_expr <|> null_lit <|> identif
  // **** Priority 0 ****
  // primary expressions

  // TODO: subs atom here
  def group: Parser[GroupBy] = "(" ~ atom ~ ")" ^^ (
    parsed =>  { val ((lb, expr), rb) = parsed; GroupBy(lb, expr, rb) }
  )

  def primary:Parser[Primary] = memberAccess <|> atom

  // (null.first).second
  def memberAccess: Parser[Primary] = atom ~ **(DOT ~ IDENTIFIER) ^^ (parsed => flatten1(parsed))
    ^^ (parsed => foldFirst(parsed)(MemberAccess)) ^^ (parsed => foldTernary(parsed)(MemberAccess))

  // **** Priority 1 ****
  // Unary expression
  def unary: Parser[Expression] = (negate <|> u_plus <|> bitwiseNot) <|> atom

  def negate: Parser[Negate]            = ("-" ~ unary) ^^ (p => Negate(p._1, p._2))
  def u_plus: Parser[UPlus]             = ("+" ~ unary) ^^ (p => UPlus(p._1, p._2))
  def bitwiseNot: Parser[BitwiseNot]    = ("~" ~ unary) ^^ (p => BitwiseNot(p._1, p._2))

  // **** Priority 2 ****

  // TODO: to another file


  def factor: Parser[Expression] = (unary ~ **(("*" <|> "/") ~ unary) ^^ mkBinary) <|> unary

  def term: Parser[Expression]   = (factor ~ **(("+" <|> "-" <|> "%") ~ factor) ^^ mkBinary) <|> factor

  def mkBinary(repr: (Expression, ((Terminal, Expression), List[(Terminal, Expression)]))): Expression = {
    val (left, ((op, right), tail)) = repr

    val first = BinaryExpression(left, op, right)

    tail.foldLeft(first)((left, term) =>
      val (op, right) = term
      BinaryExpression(left, op, right)
    )
  }

  def flatten1[A <: ParsingTree, B <: ParsingTree, C <: ParsingTree](repr: (A, ((B,C), List[(B, C)]))): (A, B, C, List[(B, C)]) = {
    val (first, ((second, third), tail)) = repr
    (first, second, third, tail)
  }

  def foldFirst[A <: ParsingTree, B <: ParsingTree, C <: ParsingTree, T <: TernaryBranch](repr: (A, B, C, List[(B, C)]))( f: (A, B, C) => T): (T, List[(B, C)]) = {
    val (a, b, c, tail) = repr
    (f(a, b, c), tail)
  }

  def foldTernary[T <: TernaryBranch, A <: ParsingTree, B <: ParsingTree](repr: (T, List[(A, B)]))(f: (T, A, B) => T): T = {
    val (first, tail) = repr
    tail.foldLeft(first)( (left, term) =>
      val (op, right) = term
      f(left, op, right)
    )
  }
}
