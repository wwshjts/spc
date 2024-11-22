package org.syspro.spc
package parse
import org.scalatest.funsuite.AnyFunSuite
import org.syspro.spc.lexer.Lexer
import org.syspro.spc.parser.grammar.Grammar.*

class ParseTest extends AnyFunSuite {
  test("test of basic parsers") {
    val input = Lexer("11 + 113")

    println(unary(input))
  }
}
