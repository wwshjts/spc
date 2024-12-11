package org.syspro.spc
package parser.token

import parser.parsing_tree.*

import syspro.tm.parser.SyntaxKind
import syspro.tm.parser.AnySyntaxKind
import syspro.tm.lexer.{BooleanLiteralToken, Keyword, Symbol}

object ParsingTreeConverter {
  def apply(tree: ParsingTree): AnySyntaxKind = tree match
    case grammar: Grammar => grammar match
      case _: SourceText => SyntaxKind.SOURCE_TEXT
      case terminal: Terminal => terminal match
        case BAD(tkn) => SyntaxKind.BAD
        case INDENT(tkn) => SyntaxKind.INDENT
        case DEDENT(tkn) => SyntaxKind.DEDENT
        case IDENTIFIER(tkn) => SyntaxKind.IDENTIFIER
        case RUNE(tkn) => SyntaxKind.RUNE
        case BOOLEAN(tkn) => SyntaxKind.BOOLEAN
        case INTEGER(tkn) => SyntaxKind.INTEGER
        case STRING(tkn) => SyntaxKind.STRING
        case DOT(tkn) => Symbol.DOT
        case COLON(tkn) => Symbol.COLON
        case COMMA(tkn) => Symbol.COMMA
        case PLUS(tkn) => Symbol.PLUS
        case MINUS(tkn) => Symbol.MINUS
        case ASTERISK(tkn) => Symbol.ASTERISK
        case SLASH(tkn) => Symbol.SLASH
        case TILDE(tkn) => Symbol.TILDE
        case PERCENT(tkn) => Symbol.PERCENT
        case OPEN_PAREN(tkn) => Symbol.OPEN_PAREN
        case CLOSE_PAREN(tkn) => Symbol.CLOSE_PAREN
        case OPEN_BRACKET(tkn) => Symbol.OPEN_BRACKET
        case CLOSE_BRACKET(tkn) => Symbol.CLOSE_BRACKET
        case AMPERSAND(tkn) => Symbol.AMPERSAND
        case CARET(tkn) => Symbol.CARET
        case BAR(tkn) => Symbol.BAR
        case QUESTION(tkn) => Symbol.QUESTION
        case LEFT(tkn) => Symbol.LESS_THAN
        case RIGHT(tkn) => Symbol.GREATER_THAN
        case EQ(tkn) => Symbol.EQUALS
        case EQ_EQ(tkn) => Symbol.EQUALS_EQUALS
        case EXCLAMATION(tkn) => Symbol.EXCLAMATION
        case NEQ(tkn) => Symbol.EXCLAMATION_EQUALS
        case LEFT_EQ(tkn) => Symbol.LESS_THAN_EQUALS
        case RIGHT_EQ(tkn) => Symbol.GREATER_THAN_EQUALS
        case LEFT_LEFT(tkn) => Symbol.LESS_THAN_LESS_THAN
        case RIGHT_RIGHT(tkn) => Symbol.GREATER_THAN_GREATER_THAN
        case AMPERSAND_AMPERSAND(tkn) => Symbol.AMPERSAND_AMPERSAND
        case BAR_BAR(tkn) => Symbol.BAR_BAR
        case BOUND(tkn) => Symbol.BOUND
        case THIS(tkn) => Keyword.THIS
        case SUPER(tkn) => Keyword.SUPER
        case NULL(tkn) => Keyword.NULL
        case IS(tkn) => Keyword.IS
        case FOR(tkn) => Keyword.FOR
        case WHILE(tkn) => Keyword.WHILE
        case IF(tkn) => Keyword.IF
        case ELSE(tkn) => Keyword.ELSE
        case IN(tkn) => Keyword.IN
        case BREAK(tkn) => Keyword.BREAK
        case CONTINUE(tkn) => Keyword.CONTINUE
        case RETURN(tkn) => Keyword.RETURN
        case VAR(tkn) => Keyword.VAR
        case VAL(tkn) => Keyword.VAL
        case DEF(tkn) => Keyword.DEF
        case ABSTRACT(tkn) => Keyword.ABSTRACT
        case VIRTUAL(tkn) => Keyword.VIRTUAL
        case OVERRIDE(tkn) => Keyword.DEF
        case NATIVE(tkn) => Keyword.NATIVE
        case CLASS(tkn) => Keyword.CLASS
        case OBJECT(tkn) => Keyword.OBJECT
        case INTERFACE(tkn) => Keyword.INTERFACE
      case expression: Expression => expression match
        case primary: Primary => primary match
          case atom: Atom => atom match
            case expr: LiteralExpr => expr match
              case StringLiteral(op) => SyntaxKind.STRING_LITERAL_EXPRESSION
              case IntegerLiteral(op) => SyntaxKind.INTEGER_LITERAL_EXPRESSION
              case RuneLiteral(op) => SyntaxKind.RUNE_LITERAL_EXPRESSION
              case NullLiteral(op) => SyntaxKind.NULL_LITERAL_EXPRESSION
              case BooleanLiteral(op) => {
                if (op.token().asInstanceOf[BooleanLiteralToken].value)
                  SyntaxKind.TRUE_LITERAL_EXPRESSION
                else
                  SyntaxKind.FALSE_LITERAL_EXPRESSION
              }
              case ThisExpr(op) => SyntaxKind.THIS_EXPRESSION
              case SuperExpr(op) => SyntaxKind.SUPER_EXPRESSION
          case name: Name => name match
            case IdentifierName(op) => SyntaxKind.IDENTIFIER_NAME_EXPRESSION
            case OptionName(op, name) => SyntaxKind.OPTION_NAME_EXPRESSION
            case GenericName(i, l, separatedList, r) => SyntaxKind.GENERIC_NAME_EXPRESSION
          case GroupBy(leftb, expr, rightb) => SyntaxKind.PARENTHESIZED_EXPRESSION
          case MemberAccess(left, dot, i) => SyntaxKind.MEMBER_ACCESS_EXPRESSION
          case Index(indexed, l, index, r) => SyntaxKind.INDEX_EXPRESSION
          case Invoke(i, lp, list, rp) => SyntaxKind.INVOCATION_EXPRESSION
        case expr: BinaryExpression => expr match
          case ADD(left, op, right) => SyntaxKind.ADD_EXPRESSION
          case SUBTRACT(left, op, right) => SyntaxKind.SUBTRACT_EXPRESSION
          case MULTIPLY(left, op, right) => SyntaxKind.MULTIPLY_EXPRESSION
          case DIV(left, op, right) => SyntaxKind.DIVIDE_EXPRESSION
          case MOD(left, op, right) => SyntaxKind.MODULO_EXPRESSION
          case LEFT_SHIFT(left, op, right) => SyntaxKind.BITWISE_LEFT_SHIFT_EXPRESSION
          case RIGHT_SHIFT(left, op, right) => SyntaxKind.BITWISE_RIGHT_SHIFT_EXPRESSION
          case BitwiseAnd(left, op, right) => SyntaxKind.BITWISE_AND_EXPRESSION
          case BitwiseOr(left, op, right) => SyntaxKind.BITWISE_OR_EXPRESSION
          case Xor(left, op, right) => SyntaxKind.BITWISE_EXCLUSIVE_OR_EXPRESSION
          case AND(left, op, right) => SyntaxKind.LOGICAL_AND_EXPRESSION
          case OR(left, op, right) => SyntaxKind.LOGICAL_OR_EXPRESSION
          case LessThan(left, op, right) => SyntaxKind.LESS_THAN_EXPRESSION
          case GreaterThan(left, op, right) => SyntaxKind.GREATER_THAN_EXPRESSION
          case LessOrEq(left, op, right) => SyntaxKind.LESS_THAN_OR_EQUAL_EXPRESSION
          case GreaterOrEq(left, op, right) => SyntaxKind.GREATER_THAN_OR_EQUAL_EXPRESSION
          case Equal(left, op, right) => SyntaxKind.EQUALS_EXPRESSION
          case NEqual(left, op, right) => SyntaxKind.NOT_EQUALS_EXPRESSION
        case expr: UnaryExpr => expr match
          case Negate(operation, operand) => SyntaxKind.UNARY_MINUS_EXPRESSION
          case UPlus(operation, operand) => SyntaxKind.UNARY_PLUS_EXPRESSION
          case BitwiseNot(operation, operand) => SyntaxKind.BITWISE_NOT_EXPRESSION
          case NOT(ex, operand) => SyntaxKind.LOGICAL_NOT_EXPRESSION
        case IsExpression(args@_*) => SyntaxKind.IS_EXPRESSION
      case statement: Statement => statement match
        case BreakStmt(op) => SyntaxKind.BREAK_STATEMENT
        case ContinueStmt(op) => SyntaxKind.CONTINUE_STATEMENT
        case ReturnStmt(args) => SyntaxKind.RETURN_STATEMENT
        case ExprStmt(expr) => SyntaxKind.EXPRESSION_STATEMENT
        case Assignment(left, op, right) => SyntaxKind.ASSIGNMENT_STATEMENT
        case ForStmt(args) => SyntaxKind.FOR_STATEMENT
        case WhileStmt(args) => SyntaxKind.WHILE_STATEMENT
        case IfStmt(args) => SyntaxKind.IF_STATEMENT
        case VarDefStmt(variableDef) => SyntaxKind.VARIABLE_DEFINITION_STATEMENT
      case definition: Definition => definition match
        case VariableDef(args) => SyntaxKind.VARIABLE_DEFINITION
        case ParameterDef(identifier, colon, name) => SyntaxKind.PARAMETER_DEFINITION
        case TypeParamDef(args) => SyntaxKind.TYPE_PARAMETER_DEFINITION
        case FunctionDef(args) => SyntaxKind.FUNCTION_DEFINITION
        case TypeDefinition(args) => SyntaxKind.TYPE_DEFINITION
      case supplementary: Supplementary => throw IllegalStateException("Supplementary doesn't have any syntax kind")
      case SeparatedList(trees@_*) => SyntaxKind.SEPARATED_LIST
      case GrammarList(trees) => SyntaxKind.LIST
      case TypeBound(bound, separatedList) => SyntaxKind.TYPE_BOUND
}
