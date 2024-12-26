package org.syspro.spc
package parser.grammar

import parser.parsing_tree.*
import org.syspro.spc.lexer.Lexer
import org.syspro.spc.parser.grammar.BasicLeafParser.eps
import org.syspro.spc.parser.parsing_tree
import syspro.tm.{WebServer, lexer}
import syspro.tm.lexer.SymbolToken
import syspro.tm.parser.{Diagnostic, ParseResult, SyntaxNode, TextSpan}

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.*


/**
 * Grammar of SysPro lang, written in my DSL of parser combinators
 */
object Grammar extends syspro.tm.parser.Parser {

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

  // Terminals


  def keyword_terminal: Parser[Terminal] = THIS <|> SUPER <|> NULL <|> IS <|> FOR <|> WHILE <|> IF <|> ELSE <|> IN
  <|> BREAK <|> CONTINUE <|> RETURN <|> VAR <|> VAL <|> OVERRIDE <|> DEF

  def symbol_terminal: Parser[Terminal] = "." <|> ":" <|> "," <|> "+" <|> "-" <|> "*" <|> "/" <|> "~" <|> "%" <|> "(" <|> ")"
  <|> "[" <|> "]" <|> "&" <|> "^" <|> "|" <|> "<" <|> ">" <|> "?" <|> "!" <|> "=" <|> "==" <|> "!=" <|> "<=" <|> ">="
  <|> "&&" <|> "||" <|> "<:"

  def terminal: Parser[Terminal] = BAD <|> INDENT <|> DEDENT <|> IDENTIFIER <|> RUNE <|> BOOLEAN <|> INTEGER <|> STRING
  <|> keyword_terminal <|> symbol_terminal

  // Name expressions
  def name: Parser[Name] =  option_name <|> generic_name <|> identifier_name
  def identifier_name: Parser[IdentifierName] = IDENTIFIER ^^ IdentifierName
  def option_name: Parser[OptionName]  = "?" ~ name ^^ { parsed =>
    val (q, name) = parsed
    OptionName(q, name)
  }

  def generic_parameters: Parser[SeparatedList] = *?(name ~ ",") ~ name ^^ { parsed =>
    val (list, expr) = parsed
    SeparatedList((expr :: list.flatten((a, b) => List(a, b)).reverse).reverse)
  }


  def soft_generic_parameters: Parser[IncompleteGenericParams] = *?(name ~ ",") ~ almost_generic ^^ { parsed =>
    val (list, almost_generic) = parsed
    IncompleteGenericParams(list.flatten((a, b) => List(a, b)), almost_generic)
  }

  def almost_generic: Parser[AlmostGeneric] = IDENTIFIER ~ "<" ~ generic_parameters ^^ { parsed =>
    val ((identifier, left), params) = parsed
    AlmostGeneric(identifier, left, params)
  }

  case class AlmostGeneric(identifier: Terminal, left: Terminal, parameters: SeparatedList)
  case class IncompleteGenericParams(params: List[ParsingTree], incomplete: AlmostGeneric)

  def generic: Parser[GenericName] = IDENTIFIER ~ "<" ~ generic_parameters ~ ">" ^^ { parsed =>
    val (((i, lt), sep), gt) = parsed
    GenericName(i, lt, sep, gt)
  }

  def nested_generic: Parser[GenericName] = IDENTIFIER ~ "<" ~ soft_generic_parameters ~ ">>" ^^ { parsed =>
    val (((i, lt), params), gt_gt) = parsed

    val gt_gt_token = gt_gt.token()
    val gt_nested = RIGHT(new SymbolToken(gt_gt_token.start, gt_gt_token.start, gt_gt_token.leadingTriviaLength, 0, lexer.Symbol.GREATER_THAN))
    val gt = RIGHT(new SymbolToken(gt_gt_token.end, gt_gt_token.end, 0, gt_gt_token.trailingTriviaLength, lexer.Symbol.GREATER_THAN))

    // construct nested generic
    val incomplete = params.incomplete
    val nested_generic = GenericName(incomplete.identifier, incomplete.left, incomplete.parameters, gt_nested)

    // construct generic name
    val complete_params = SeparatedList((nested_generic :: params.params.reverse).reverse)

    GenericName(i, lt, complete_params, gt)
  }

  def generic_name: Parser[GenericName] = generic <|> nested_generic 

  def separatedList_expr_comma: Parser[SeparatedList] = *?(expression ~ ",") ~ expression ^^ { parsed =>
    val (list, expr) = parsed
    SeparatedList((expr :: list.flatten((a, b) => List(a, b)).reverse).reverse)
  }

  def separatedList_name_amper: Parser[SeparatedList] = *?(name ~ "&") ~ name ^^ { parsed =>
    val (list, amper) = parsed
    SeparatedList((amper :: list.flatten((a, b) => List(a, b)).reverse).reverse)
  }

  def separatedList_parameterDef_comma: Parser[SeparatedList] = *?(parameter_def ~ ",") ~ parameter_def ^^ { parsed =>
    val (list, comma) = parsed
    SeparatedList((comma :: list.flatten((a, b) => List(a, b)).reverse).reverse )
  }

  def separatedList_typeParameterDef_comma: Parser[SeparatedList] = *?(type_param_def ~ ",") ~ type_param_def ^^ { parsed =>
    val (list, comma) = parsed
    SeparatedList((comma :: list.flatten((a, b) => List(a, b)).reverse).reverse)
  }

  def type_bound: Parser[TypeBound]  = "<:" ~ separatedList_name_amper ^^ (parsed => TypeBound(parsed._1, parsed._2))

  def atom: Parser[Primary] = integer <|> string <|> bool <|> rune <|> this_expr
                                            <|> super_expr <|> null_lit <|> name

  // **** Priority 0 ****

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

  def eps_empty_list: Parser[SeparatedList] = eps ^^ { parsed =>
   SeparatedList(List.empty)
  }
  def _invoke: Parser[PrimaryContainer] = "(" ~ (separatedList_expr_comma <|> eps_empty_list) ~ ")" ^^ (parsed => InvokeContainer(parsed._1._1, parsed._1._2, parsed._2))

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
  // **** Priority 9 ****
  def logical_not: Parser[Expression] = ("!" ~ expression ^^ ( parsed => NOT(parsed._1, parsed._2) )) <|> comparsion

  // **** Priority 10 ****
  def and: Parser[Expression] = logical_not ~ *?("&&" ~ logical_not) ^^ _mkBinary

  // **** Priority 11 ****
  def or: Parser[Expression] = and ~ *?("||" ~ and) ^^ _mkBinary

  def expression: Parser[Expression] = or

  // **** Statements ****

  def for_loop: Parser[Statement] = FOR ~ primary ~ IN ~ expression ~ ?(block(statement)) ^^ (parsed =>
    val ((((cycle, elem), in), collection), block) = parsed

    ForStmt(cycle, elem, in, collection, block)
  )

  def while_loop: Parser[Statement] = WHILE ~ expression ~ ?(block(statement)) ^^ (parsed =>
    val ((cycle, cond), block) = parsed
    WhileStmt(cycle, cond, block)
  )

  def assignment: Parser[Statement] = primary ~ "=" ~ expression ^^ (parsed =>
    val ((l, eq), r) = parsed
    Assignment(l, eq, r)
  )

  def if_stmt: Parser[Statement] = IF ~ expression ~ ?(block(statement)) ~ ?(ELSE ~ ?(block(statement))) ^^ {
    parsed =>
      val (((if_stmt, expr), block), else_stmt) = parsed

      IfStmt(if_stmt, expr, block, else_stmt)
  }

  def return_stmt: Parser[Statement]        = RETURN ~ ?(expression) ^^ (parsed => ReturnStmt(parsed._1, parsed._2))
  def break_stmt: Parser[Statement]         = BREAK ^^ BreakStmt
  def continue_stmt: Parser[Statement]      = CONTINUE ^^ ContinueStmt
  def expression_stmt: Parser[Statement]    = expression ^^ ExprStmt
  def variable_def_stmt: Parser[Statement]  = variable_def ^^ VarDefStmt

  def statement: Parser[Statement] = while_loop <|> for_loop <|> assignment <|> if_stmt <|> return_stmt <|> break_stmt
                                    <|> continue_stmt <|> expression_stmt <|> variable_def_stmt

  // **** Definitions ****
  def variable_def: Parser[VariableDef] = (VAL <|> VAR) ~ IDENTIFIER ~ ?(COLON ~ name) ~ ?("=" ~ expression) ^^ { parsed =>
      val (((md, identifier), type_name), assignment_opt) = parsed

      VariableDef(md, identifier, type_name, assignment_opt)
  }

  def parameter_def: Parser[Definition] = IDENTIFIER ~ COLON ~ name ^^ {
    parsed =>
      val ((identifier, colon), name) = parsed
      ParameterDef(identifier, colon, name)
  }

  def type_param_def: Parser[Definition] = IDENTIFIER ~ ?(type_bound) ^^ {
    parsed => TypeParamDef(parsed._1, parsed._2)
  }

  def func_mods: Parser[GrammarList] = *?(ABSTRACT <|> VIRTUAL <|> OVERRIDE <|> NATIVE) ^^ GrammarList
  def function_def: Parser[FunctionDef] =
    func_mods ~ DEF ~ (IDENTIFIER <|> THIS) ~ "(" ~ ?(separatedList_parameterDef_comma) ~ ")" ~ ?(":" ~ name) ~ ?(block(statement)) ^^ { parsed =>

    val (((((((mod, df), name), op), args), cp), ret_type_opt), body) = parsed

    FunctionDef(mod, df, name, op, args, cp, ret_type_opt, body)
  }

  def type_def: Parser[TypeDefinition] = (CLASS <|> OBJECT <|> INTERFACE) ~
    IDENTIFIER ~ ?("<" ~ separatedList_typeParameterDef_comma ~ ">") ~ ?(type_bound) ~ ?(block(function_def <|> variable_def)) ^^ { parsed =>

    // TODO: merge Separated list and grammar list nodes add seplist block to rule
    val ((((mod, name), seplist_opt), bound_opt), block)  = parsed

    val sepList = seplist_opt.map(p => p._1._1 :: p._1._2 :: p._2 :: Nil).getOrElse(List.empty)

    TypeDefinition(mod, name, sepList, bound_opt, block)
  }

  def source_text: Parser[ParsingTree] = |**(type_def) ^^ { parsed =>
    SourceText(GrammarList(parsed))
  }

  def block[A <: ParsingTree](p: Parser[A]): Parser[Block] = INDENT ~ (|**(p) ^^ GrammarList) ~ DEDENT ^^ { parsed =>
    val ((indent, list), dedent) = parsed
    Block(indent, list, dedent)
  }

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

  case class PResult(root: SyntaxNode, invalidRanges: java.util.List[TextSpan], diagnostics: java.util.List[Diagnostic]) extends ParseResult
  override def parse(s: String): ParseResult = {
    val res = source_text(Lexer(s))

    var tree: SyntaxNode = null
    val invalidRanges = res match
      case Success(result, remain_input) =>
        tree = res.get
        if (res.remain.nonEmpty)
          (new TextSpan(res.remain.head.start, res.remain.last.end - res.remain.head.start + 1)) :: Nil
        else
          List.empty
      case Failure(msg, remain_input) =>
        tree = SourceText(GrammarList(List.empty))
        (new TextSpan(0, s.codePoints().count().toInt)) :: Nil

    PResult(tree, invalidRanges.asJava, List.empty.asJava)
  }
}
