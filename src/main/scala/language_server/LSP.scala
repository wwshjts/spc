package org.syspro.spc
package language_server

import syspro.tm.{lexer, symbols}
import syspro.tm.lexer.{Keyword, Token}
import lexer.Lexer
import parser.grammar.Grammar
import parser.grammar.Grammar.{PResult, expression, variable_def}

import org.syspro.spc.parser.parsing_tree.{FunctionDef, GenericName, IdentifierName, OptionName, PTree, VariableDef, IDENTIFIER}
import syspro.tm.parser.{Diagnostic, SyntaxKind, SyntaxNode, TextSpan}
import syspro.tm.symbols.{SemanticSymbol, SymbolKind, TypeSymbol}

import java.util
import scala.collection.mutable

/**
 * LanguageServerPro for `SysProLang`
 */
object LSP extends symbols.LanguageServer {
  val builtInTypes: Set[String] = Set("Int32", "Int64", "UInt32", "UInt64", "Boolean", "Rune", "Object", "Iterable", "Iterator", "Array", "String")
  case class SemanticModel(parseResult: PResult) extends symbols.SemanticModel {
    override def root(): SyntaxNode = ???

    override def invalidRanges(): util.Collection[TextSpan] = ???

    override def diagnostics(): util.Collection[Diagnostic] = ???

    override def typeDefinitions(): util.List[_ <: TypeSymbol] = ???

    override def lookupType(name: String): TypeSymbol = ???
  }

  type Scope = TypeSemantic | FunctionSemantic

  extension (s: Scope) {
    def addMember(variableSemantic: VariableSemantic): Unit = s match
      case t: TypeSemantic => t.members.add(variableSemantic)
      case f: FunctionSemantic => f.locals.add(variableSemantic)
  }

  extension  (s: Scope) {
    def get: SemanticSymbol = s match
      case t: TypeSemantic => t
      case f: FunctionSemantic => f
  }

  type NameTable = mutable.Map[PTree, SemanticSymbol]
  /**
   * Context of LSP
   *
   * Used to transfer state of LSP between LSP functions
   */
  case class Context(scope: TypeSemantic)
  override def buildModel(code: String): symbols.SemanticModel = {
    val resultOfParsing = Grammar.parse(code)
    val tree = resultOfParsing.root()

    // List of all type definitions that we seen
    val typeDefinitions = mutable.ListBuffer.empty

    ???
  }

  def lookUp(): TypeSymbol = ???


  /**
   *
   * @param functionDef functionDefinition without any semantic
   * @return new `FunctionDef` that actually have semantic
   */
  def concreteFunction(functionDef: FunctionDef): FunctionDef = {
    ???
  }

  /**
   *
   * @param variableDef variable definition without semantic
   * @return `VariableDef` with computed semantic
   */
  def concreteVariable(variableDef: VariableDef)(using scope: Scope): VariableDef = {
    val typeName = variableDef.type_name match {
      case IdentifierName(op) => op.asInstanceOf[IDENTIFIER].tkn.toString
      case OptionName(op, name) => ???
      case GenericName(i, l, separatedList, r) => i.asInstanceOf[IDENTIFIER].tkn.toString
    }


    val typeSemantic = if builtInTypes contains typeName then null else lookUp()

    val semantic = VariableSemantic(
      `type` = typeSemantic,
      kind = {
        scope match
          case t: TypeSemantic => SymbolKind.FIELD
          case f: FunctionSemantic => SymbolKind.LOCAL
      },
      name = variableDef.identifier.toString,
      definition = variableDef,
      owner = scope.get
    )

    scope.addMember(semantic)

    variableDef.addSemantic(semantic)
  }
}

