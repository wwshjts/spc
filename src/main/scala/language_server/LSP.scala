package org.syspro.spc
package language_server

import syspro.tm.{lexer, symbols}
import syspro.tm.lexer.{Keyword, Token}
import lexer.Lexer
import parser.grammar.Grammar
import parser.grammar.Grammar.{PResult, expression, variable_def}

import org.syspro.spc.parser.parsing_tree.{FunctionDef, GenericName, IDENTIFIER, IdentifierName, NullName, PTree, SourceText, TypeDefinition, VariableDef}
import syspro.tm.parser.{Diagnostic, SyntaxKind, SyntaxNode, TextSpan}
import syspro.tm.symbols.{SemanticSymbol, SymbolKind, TypeSymbol, VariableSymbol}

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

  override def buildModel(code: String): symbols.SemanticModel = {
    val resultOfParsing = Grammar.parse(code)
    val tree = resultOfParsing.root()

    // List of all type definitions that we seen
    val typeDefinitions = mutable.ListBuffer.empty

    ???
  }

  def buildSemanticModel(root: SourceText): symbols.SemanticModel = {
    ???
  }

  private def buildNameTable(types: List[TypeDefinition]): Map[String, TypeDefinition] =
    types.map(t => t.identifier.token().toString -> t).toMap

  def extractTypes(types: List[TypeDefinition]): List[Type] = {

    /*
    case class RawType(tp: Type, directBaseNames: List[String]) {
      def isDerived: Boolean = directBaseNames.nonEmpty
      def numOfDirectBaseTypes: Int = directBaseNames.length
      def name: String = tp.name()
    }

    case class Context private (unresolved: mutable.Map[String, RawType], nameTable: mutable.Map[String, RawType]) {
      def register(rt: RawType): Unit = {
        for (directBase <- rt.directBaseNames) { unresolved += directBase -> rt }
        nameTable += rt.name -> rt
      }
      def resolved: Boolean = unresolved.empty
    }

    case object Context {
      def empty: Context = new Context(mutable.Map.empty, mutable.Map.empty)
    }

    case object RawType {
      def apply(typeDefinition: TypeDefinition)(using ctx: Context): RawType = {
        ???
      }
    }

    def extractRaw(types: List[TypeDefinition])(using ctx: Context): List[RawType] = types match
      case head :: next => RawType(head) :: extractRaw(next)
      case Nil => Nil

    /**
     * @param name
     * @param ctx
     * @return
     */
    def resolveBase(name: String)(using ctx: Context): Option[Type] = {
      if ctx.unresolved contains name then
        val rawType = ctx.unresolved(name)
        if !rawType.isDerived then
          ctx.resolved += rawType.name -> rawType.tp
          Some(rawType.tp)
        else
          val baseTypes = rawType.directBaseNames.map(resolveBase).filter(_.nonEmpty).map(_.get)
          Some(rawType.tp.ofBaseTypes(baseTypes))
      else
        ???
    }

     */
    ???
  }


}

