package org.syspro.spc

import org.sypro.spc.lexer.SpcLexer
import org.syspro.spc.parser.grammar.Grammar
import syspro.tm.Tasks
import syspro.tm.lexer.TestMode

@main
def main(): Unit = {
  //Tasks.Lexer.registerSolution(new SpcLexer(),  new TestMode().strict(true))
  Tasks.Parser.registerSolution(Grammar)
}

