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
  def null_i: Parser[IdentifierName]    = NULL ^^ IdentifierName
  def this_expr: Parser[ThisExpr]       = THIS ^^ ThisExpr
  def super_expr: Parser[SuperExpr]     = SUPER ^^ SuperExpr
  def identif: Parser[IdentifierName]   = IDENTIFIER ^^ IdentifierName

  def separatedList_expr_comma: Parser[SeparatedList] = (*?(expression ~ ",")) ~ expression ^^ (parsed =>
      val (list, expr) = parsed
      SeparatedList((expr :: list.flatten((a, b) => List(a, b)).reverse).reverse*)
    )

  def atom: Parser[Atom] = integer <|> string <|> bool <|> rune <|> this_expr
                                            <|> super_expr <|> null_lit <|> identif
  // **** Priority 0 ****
  // primary expressions

  def group: Parser[Primary] = ("(" ~ expression ~ ")" ^^ (
    parsed =>  { val ((lb, expr), rb) = parsed; GroupBy(lb, expr, rb) }
  )) <|> atom

  def primary:Parser[Primary] =  invoke <|> index_expr <|> memberAccess <|> group <|> atom

  // (null.first).second
  def memberAccess: Parser[Primary] = (group ~ **(DOT ~ IDENTIFIER)
    ^^ (parsed => flatten1(parsed)) ^^ (parsed => foldFirst(parsed)(MemberAccess)) ^^ (parsed => foldTernary(parsed)(MemberAccess))) <|> group

  def index_expr: Parser[Primary] = (memberAccess ~ "[" ~ term ~ "]" ^^ ( parsed =>
    val (((indexed, lb), expr), rb) = parsed
    Index(indexed, lb, expr, rb)
  )) <|> memberAccess

  def invoke: Parser[Primary] = ((index_expr ~ "(" ~ separatedList_expr_comma ~ ")") ^^ ( parsed =>
    val (((inv, lb), sep_list), rb) = parsed
    Invoke(inv, lb, sep_list, rb)
  )) <|> index_expr

  // **** Priority 1 ****
  // Unary expression
  def unary: Parser[Expression] = (negate <|> u_plus <|> bitwiseNot) <|> invoke

  def negate: Parser[Negate]            = ("-" ~ unary) ^^ (p => Negate(p._1, p._2))
  def u_plus: Parser[UPlus]             = ("+" ~ unary) ^^ (p => UPlus(p._1, p._2))
  def bitwiseNot: Parser[BitwiseNot]    = ("~" ~ unary) ^^ (p => BitwiseNot(p._1, p._2))

  // **** Priority 2 ****
  def factor: Parser[Expression] = (unary ~ **(("*" <|> "/") ~ unary) ^^ mkBinary) <|> unary

  // **** Priority 3 ****
  def term: Parser[Expression]   = (factor ~ **(("+" <|> "-" <|> "%") ~ factor) ^^ mkBinary) <|> factor

  // **** Priority 4 ****
  def shift: Parser[Expression] = (term ~ **(("<<" <|> ">>")  ~ term) ^^ mkBinary) <|> term

  // **** Priority 5 ****


  def expression: Parser[Expression] = term
  // **********

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
