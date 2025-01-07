package org.syspro.spc

import org.syspro.spc.parser.grammar.Grammar
import syspro.tm.{Tasks, WebServer}
import syspro.tm.lexer.TestMode
import syspro.tm.symbols.{LanguageServer, SemanticModel}

@main
def main(): Unit = {
  //Tasks.Lexer.registerSolution(new SpcLexer(),  new TestMode().strict(true))
//  WebServer.start()
  // Tasks.Parser.registerSolution(Grammar)
//  WebServer.waitForWebServerExit()
  class FakeLsp extends LanguageServer {
    override def buildModel(code: String): SemanticModel = {
      println(code)
      ???
    }
  }

  Tasks.LanguageServer.registerSolution(FakeLsp())
}

