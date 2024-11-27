package org.syspro.spc
package parse
import org.scalatest.funsuite.AnyFunSuite
import org.syspro.spc.lexer.Lexer
import org.syspro.spc.parser.grammar.Grammar.*
import org.syspro.spc.parser.grammar.{Combinators, Parser}

class ParseTest extends AnyFunSuite {
  test("test of basic parsers") {
    val input = Lexer("123i64 * 42i32 / 11")

    println(factor(input))
  }
}
