package org.syspro.spc
package parse

import org.scalatest.funsuite.AnyFunSuite
import org.syspro.spc.lexer.Lexer
import org.syspro.spc.parser.grammar.Grammar.*
import org.syspro.spc.parser.grammar.Success
import org.syspro.spc.parser.parsing_tree.*


class Definitions extends AnyFunSuite{
  test("variable") {
    val input = Lexer("val a: Integer = 1 + 2 * 3")
    val res = variable_def_stmt(input)
  }
}
