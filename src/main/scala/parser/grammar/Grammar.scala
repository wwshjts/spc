package org.syspro.spc
package parser.grammar

import parser.parsing_tree.*

import org.syspro.spc.parser.grammar.BasicLeafParser.eps
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

  // Name expressions
  def name: Parser[Name] =  option_name <|> generic_name <|> identifier_name
  def identifier_name: Parser[IdentifierName]   = IDENTIFIER ^^ IdentifierName
  def option_name: Parser[OptionName]           = "?" ~ name ^^ ( parsed =>
    val (q, name) = parsed
    OptionName(q, name)
  )
  def generic_name: Parser[GenericName]         = IDENTIFIER ~ "<" ~ separatedList_name_comma ~ ">" ^^ (parsed =>
    val (((i, lt), sep), gt) = parsed
    GenericName(i, lt, sep, gt)
  )

  def separatedList_expr_comma: Parser[SeparatedList] = (*?(expression ~ ",")) ~ expression ^^ (parsed =>
      val (list, expr) = parsed
      SeparatedList((expr :: list.flatten((a, b) => List(a, b)).reverse).reverse*)
    )

  def separatedList_name_comma: Parser[SeparatedList] = (*?(name ~ ",")) ~ name ^^ (parsed =>
      val (list, expr) = parsed
      SeparatedList((expr :: list.flatten((a, b) => List(a, b)).reverse).reverse*)
    )

  def atom: Parser[Primary] = integer <|> string <|> bool <|> rune <|> this_expr
                                            <|> super_expr <|> null_lit <|> name


  // **** Priority 0 ****
  //def primary:Parser[Primary] =  invoke <|> index_expr <|> memberAccess <|> group <|> atom

  // Made elimination of left recursion in primary expressions

  // all non left-recursive productions of primary
  //def primary:Parser[Primary] = group ~ _primary  <|> atom ~ _primary ^^ ???

  // Non left-recursive primary rule
  // This grammar transformation is fully equivalent to ordinary left recursion reduction
  def primary: Parser[Primary] = (atom <|> group) ~ *?(_primary)
    ^^ (parsed =>
      val (left, kleene_star) = parsed
      kleene_star match {
        case List() => left
        case head :: tail =>
          val first = head(left)

          tail.foldLeft(first)((left, container) => container(left))
      }
    )


  private def _primary: Parser[PrimaryContainer] = _memberAccess <|> _index_expr <|> _invoke

  private def group: Parser[Primary] = "(" ~ expression ~ ")" ^^ (parsed => GroupBy(parsed._1._1, parsed._1._2, parsed._2))

  private def _memberAccess: Parser[PrimaryContainer] = DOT ~ IDENTIFIER ^^ (parsed => MemberAccessContainer(parsed._1, parsed._2))

  private def _index_expr: Parser[PrimaryContainer] = "[" ~ expression ~ "]" ^^ (parsed => IndexExprContainer(parsed._1._1, parsed._1._2, parsed._2))

  private def _invoke: Parser[PrimaryContainer] = "(" ~ separatedList_expr_comma ~ ")" ^^ (parsed => InvokeContainer(parsed._1._1, parsed._1._2, parsed._2))

  // **** Priority 1 ****
  // Unary expression
  def unary: Parser[Expression] = (negate <|> u_plus <|> bitwiseNot) <|> primary

  def negate: Parser[Negate]            = ("-" ~ unary) ^^ (p => Negate(p._1, p._2))
  def u_plus: Parser[UPlus]             = ("+" ~ unary) ^^ (p => UPlus(p._1, p._2))
  def bitwiseNot: Parser[BitwiseNot]    = ("~" ~ unary) ^^ (p => BitwiseNot(p._1, p._2))

  // **** Priority 2 ****
  def factor: Parser[Expression] = unary ~ *?(("*" <|> "/") ~ unary) ^^ _mkBinary

  // **** Priority 3 ****
  def term: Parser[Expression]   = factor ~ *?(("+" <|> "-" <|> "%") ~ factor) ^^ _mkBinary

  // **** Priority 4 ****
  def shift: Parser[Expression] = term ~ *?(("<<" <|> ">>")  ~ term) ^^ _mkBinary

  // **** Priority 5 ****
  def bitwiseAnd: Parser[Expression] = shift ~ *?("&" ~ shift) ^^ _mkBinary

  // **** Priority 6 ****
  def xor: Parser[Expression] = bitwiseAnd ~ *?("^" ~ bitwiseAnd) ^^ _mkBinary

  // **** Priority 7 ****
  def bitwiseOr: Parser[Expression] = xor ~ *?("|" ~ xor) ^^ _mkBinary

  // **** Priority 8 ****
  // TODO: non effective grammar but I don't care
  def _is: Parser[IsContainer] = IS ~ name ~ ?(IDENTIFIER) ^^ (parsed =>
    val ((is, name), opt) = parsed
    IsContainer(is, name, opt)
  )
  def _cmp_term: Parser[CmpContainer] = |**(("<" <|> "<=" <|> ">" <|> ">=" <|> "==" <|> "!=") ~ bitwiseOr) ^^ CmpContainer
  def _comparison: Parser[ComparisonContainer] = _is <|> _cmp_term

  def comparsion: Parser[Expression] = bitwiseOr ~ *?(_comparison) ^^ (parsed =>
    val (left, kleene_star) = parsed

    kleene_star match
      case List() => left
      case head :: tail => {
        val first = head(left)
        tail.foldLeft(first)((left, container) => container(left))
      }

    )

  // **** Priority 10 ****
  def and: Parser[Expression] = shift ~ *?("&&" ~ shift) ^^ _mkBinary

  def expression: Parser[Expression] = comparsion
  // **********

  def _mkBinary(repr: (Expression, List[(Terminal, Expression)])): Expression = {
    val (left, kleene_star) = repr

    kleene_star match
      case List() => left // if kleene star combinator matches nothing the empty List is returned
      case head :: tail => {
        val (op, right) = head
        val first = BinaryExpression(left, op, right)
        tail.foldLeft(first)((left, term) =>
          val (op, right) = term
          BinaryExpression(left, op, right)
        )
      }
  }

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

  sealed trait ComparisonContainer {
    def apply(left: Expression): Expression = this match
      case IsContainer(is, name, opt) => opt match
        case Some(value)  => IsExpression(left, is, name, value)
        case None         => IsExpression(left, is, name)
      case CmpContainer(list) => _mkBinary((left, list))
  }
  case class IsContainer(is: Terminal, name: Name, opt: Option[Terminal]) extends ComparisonContainer
  case class CmpContainer(list: List[(Terminal, Expression)])             extends ComparisonContainer


  sealed trait PrimaryContainer {
    def apply(left: Primary): Primary = {
      this match
        case MemberAccessContainer(dot, i) => MemberAccess(left, dot, i)
        case IndexExprContainer(lb, expr, rb) => Index(left, lb, expr, rb)
        case InvokeContainer(lb, sep_list, rb) => Invoke(left, lb, sep_list, rb)
    }
  }

  case class MemberAccessContainer(dot: Terminal, i: Terminal)                    extends PrimaryContainer

  case class IndexExprContainer(lb: Terminal, expr: Expression, rb: Terminal)     extends PrimaryContainer

  case class InvokeContainer(lb: Terminal, sep_list: SeparatedList, rb: Terminal) extends PrimaryContainer

}
