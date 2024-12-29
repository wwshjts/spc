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
        case _: BAD => SyntaxKind.BAD
        case _: INDENT => SyntaxKind.INDENT
        case _: DEDENT => SyntaxKind.DEDENT
        case _: IDENTIFIER => SyntaxKind.IDENTIFIER
        case _: RUNE => SyntaxKind.RUNE
        case _: BOOLEAN => SyntaxKind.BOOLEAN
        case _: INTEGER => SyntaxKind.INTEGER
        case _: STRING => SyntaxKind.STRING
        case _: DOT => Symbol.DOT
        case _: COLON => Symbol.COLON
        case _: COMMA => Symbol.COMMA
        case _: PLUS => Symbol.PLUS
        case _: MINUS => Symbol.MINUS
        case _: ASTERISK => Symbol.ASTERISK
        case _: SLASH => Symbol.SLASH
        case _: TILDE => Symbol.TILDE
        case _: PERCENT => Symbol.PERCENT
        case _: OPEN_PAREN => Symbol.OPEN_PAREN
        case _: CLOSE_PAREN => Symbol.CLOSE_PAREN
        case _: OPEN_BRACKET => Symbol.OPEN_BRACKET
        case _: CLOSE_BRACKET => Symbol.CLOSE_BRACKET
        case _: AMPERSAND => Symbol.AMPERSAND
        case _: CARET => Symbol.CARET
        case _: BAR => Symbol.BAR
        case _: QUESTION => Symbol.QUESTION
        case _: LEFT => Symbol.LESS_THAN
        case _: RIGHT => Symbol.GREATER_THAN
        case _: EQ => Symbol.EQUALS
        case _: EQ_EQ => Symbol.EQUALS_EQUALS
        case _: EXCLAMATION => Symbol.EXCLAMATION
        case _: NEQ => Symbol.EXCLAMATION_EQUALS
        case _: LEFT_EQ => Symbol.LESS_THAN_EQUALS
        case _: RIGHT_EQ => Symbol.GREATER_THAN_EQUALS
        case _: LEFT_LEFT => Symbol.LESS_THAN_LESS_THAN
        case _: RIGHT_RIGHT => Symbol.GREATER_THAN_GREATER_THAN
        case _: AMPERSAND_AMPERSAND => Symbol.AMPERSAND_AMPERSAND
        case _: BAR_BAR => Symbol.BAR_BAR
        case _: BOUND => Symbol.BOUND
        case _: THIS => Keyword.THIS
        case _: SUPER => Keyword.SUPER
        case _: NULL => Keyword.NULL
        case _: IS => Keyword.IS
        case _: FOR => Keyword.FOR
        case _: WHILE => Keyword.WHILE
        case _: IF => Keyword.IF
        case _: ELSE => Keyword.ELSE
        case _: IN => Keyword.IN
        case _: BREAK => Keyword.BREAK
        case _: CONTINUE => Keyword.CONTINUE
        case _: RETURN => Keyword.RETURN
        case _: VAR => Keyword.VAR
        case _: VAL => Keyword.VAL
        case _: DEF => Keyword.DEF
        case _: ABSTRACT => Keyword.ABSTRACT
        case _: VIRTUAL => Keyword.VIRTUAL
        case _: OVERRIDE => Keyword.DEF
        case _: NATIVE => Keyword.NATIVE
        case _: CLASS => Keyword.CLASS
        case _: OBJECT => Keyword.OBJECT
        case _: INTERFACE => Keyword.INTERFACE
      case expression: Expression => expression match
        case primary: Primary => primary match
          case atom: Atom => atom match
            case expr: LiteralExpr => expr match
              case StringLiteral(_) => SyntaxKind.STRING_LITERAL_EXPRESSION
              case IntegerLiteral(_) => SyntaxKind.INTEGER_LITERAL_EXPRESSION
              case RuneLiteral(_) => SyntaxKind.RUNE_LITERAL_EXPRESSION
              case NullLiteral(_) => SyntaxKind.NULL_LITERAL_EXPRESSION
              case BooleanLiteral(op) => {
                if (op.token().asInstanceOf[BooleanLiteralToken].value)
                  SyntaxKind.TRUE_LITERAL_EXPRESSION
                else
                  SyntaxKind.FALSE_LITERAL_EXPRESSION
              }
              case ThisExpr(_) => SyntaxKind.THIS_EXPRESSION
              case SuperExpr(_) => SyntaxKind.SUPER_EXPRESSION
          case name: Name => name match
            case _: IdentifierName => SyntaxKind.IDENTIFIER_NAME_EXPRESSION
            case _: OptionName => SyntaxKind.OPTION_NAME_EXPRESSION
            case _: GenericName => SyntaxKind.GENERIC_NAME_EXPRESSION
          case _: GroupBy => SyntaxKind.PARENTHESIZED_EXPRESSION
          case _: MemberAccess => SyntaxKind.MEMBER_ACCESS_EXPRESSION
          case _: Index => SyntaxKind.INDEX_EXPRESSION
          case _: Invoke => SyntaxKind.INVOCATION_EXPRESSION
        case expr: BinaryExpression => expr match
          case _: ADD => SyntaxKind.ADD_EXPRESSION
          case _: SUBTRACT => SyntaxKind.SUBTRACT_EXPRESSION
          case _: MULTIPLY => SyntaxKind.MULTIPLY_EXPRESSION
          case _: DIV => SyntaxKind.DIVIDE_EXPRESSION
          case _: MOD => SyntaxKind.MODULO_EXPRESSION
          case _: LEFT_SHIFT => SyntaxKind.BITWISE_LEFT_SHIFT_EXPRESSION
          case _: RIGHT_SHIFT => SyntaxKind.BITWISE_RIGHT_SHIFT_EXPRESSION
          case _: BitwiseAnd => SyntaxKind.BITWISE_AND_EXPRESSION
          case _: BitwiseOr => SyntaxKind.BITWISE_OR_EXPRESSION
          case _: Xor => SyntaxKind.BITWISE_EXCLUSIVE_OR_EXPRESSION
          case _: AND => SyntaxKind.LOGICAL_AND_EXPRESSION
          case _: OR => SyntaxKind.LOGICAL_OR_EXPRESSION
          case _: LessThan => SyntaxKind.LESS_THAN_EXPRESSION
          case _: GreaterThan => SyntaxKind.GREATER_THAN_EXPRESSION
          case _: LessOrEq => SyntaxKind.LESS_THAN_OR_EQUAL_EXPRESSION
          case _: GreaterOrEq => SyntaxKind.GREATER_THAN_OR_EQUAL_EXPRESSION
          case _: Equal => SyntaxKind.EQUALS_EXPRESSION
          case _: NEqual => SyntaxKind.NOT_EQUALS_EXPRESSION
        case expr: UnaryExpr => expr match
          case _: Negate => SyntaxKind.UNARY_MINUS_EXPRESSION
          case _: UPlus => SyntaxKind.UNARY_PLUS_EXPRESSION
          case _: BitwiseNot => SyntaxKind.BITWISE_NOT_EXPRESSION
          case _: NOT => SyntaxKind.LOGICAL_NOT_EXPRESSION
        case _: IsExpression => SyntaxKind.IS_EXPRESSION
      case statement: Statement => statement match
        case _: BreakStmt => SyntaxKind.BREAK_STATEMENT
        case _: ContinueStmt => SyntaxKind.CONTINUE_STATEMENT
        case _: ReturnStmt => SyntaxKind.RETURN_STATEMENT
        case _: ExprStmt => SyntaxKind.EXPRESSION_STATEMENT
        case _: Assignment => SyntaxKind.ASSIGNMENT_STATEMENT
        case _: ForStmt => SyntaxKind.FOR_STATEMENT
        case _: WhileStmt => SyntaxKind.WHILE_STATEMENT
        case _: IfStmt => SyntaxKind.IF_STATEMENT
        case _: VarDefStmt => SyntaxKind.VARIABLE_DEFINITION_STATEMENT
      case definition: Definition => definition match
        case _: VariableDef => SyntaxKind.VARIABLE_DEFINITION
        case _: ParameterDef => SyntaxKind.PARAMETER_DEFINITION
        case _: TypeParamDef => SyntaxKind.TYPE_PARAMETER_DEFINITION
        case _: FunctionDef => SyntaxKind.FUNCTION_DEFINITION
        case _: TypeDefinition => SyntaxKind.TYPE_DEFINITION
      case supplementary: Supplementary => throw IllegalStateException("Supplementary doesn't have any syntax kind")
      case proto: ProtoList => proto match
        case _: SeparatedList => SyntaxKind.SEPARATED_LIST
        case _: GrammarList   => SyntaxKind.LIST
      case _: TypeBound => SyntaxKind.TYPE_BOUND
}
