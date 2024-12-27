package org.syspro.spc
package language_server

import syspro.tm.symbols
import syspro.tm.lexer.Token
import lexer.Lexer
import parser.grammar.Grammar
import parser.grammar.Grammar.PResult

import org.syspro.spc.parser.parsing_tree.ParsingTree
import syspro.tm.parser.{Diagnostic, SyntaxNode, TextSpan}
import syspro.tm.symbols.TypeSymbol

import java.util

/**
 * LanguageServerPro for `SysProLang`
 */
object LSP extends symbols.LanguageServer {
  case class SemanticModel(parseResult: PResult) extends symbols.SemanticModel {
    override def root(): SyntaxNode = ???

    override def invalidRanges(): util.Collection[TextSpan] = ???

    override def diagnostics(): util.Collection[Diagnostic] = ???

    override def typeDefinitions(): util.List[_ <: TypeSymbol] = ???

    override def lookupType(name: String): TypeSymbol = ???
  }

  /**
   * Context of LSP
   *
   * Used to transfer state of LSP between LSP functions
   */
  case class Context()
  override def buildModel(code: String): symbols.SemanticModel = {
    val resultOfParsing = Grammar.parse(code)

    ???
  }

}

