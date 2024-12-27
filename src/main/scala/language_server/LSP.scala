package org.syspro.spc
package language_server

import syspro.tm.symbols
import syspro.tm.lexer.Token
import lexer.Lexer
import parser.grammar.Grammar
import parser.grammar.Grammar.PResult

import org.syspro.spc.parser.parsing_tree.ParsingTree

/**
 * LanguageServerPro for `SysProLang`
 */
object LSP extends symbols.LanguageServer {
  case class SemanticModel(parseResult: PResult) extends symbols.SemanticModel {
    private val root = parseResult.root
    private val invalidRanges = parseResult.invalidRanges
    private val diagnostics = parseResult.diagnostics
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

