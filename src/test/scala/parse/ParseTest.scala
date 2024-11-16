package org.syspro.spc
package parse
import org.scalatest.funsuite.AnyFunSuite
import org.syspro.spc.lexer.Lexer
import org.syspro.spc.parser.grammar.*
import org.syspro.spc.parser.parsing_tree.Leaf

class ParseTest extends AnyFunSuite {
  test("test of basic parsers") {
    val input = Lexer("123")
    val tkn = input.head

    val result = integer(input)

  }

  test("andThen combinator") {
    val input = Lexer("123i32")

    import ParserCombinators.*

  }
}
