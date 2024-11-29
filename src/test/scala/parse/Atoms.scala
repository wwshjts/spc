package org.syspro.spc
package parse

import org.scalatest.funsuite.AnyFunSuite
import org.syspro.spc.lexer.Lexer
import org.syspro.spc.parser.grammar.Grammar.*
import org.syspro.spc.parser.grammar.{Combinators, Parser}
import parser.parsing_tree.*

class Atoms extends AnyFunSuite {
  test("integer") {
    val integers_lex = Lexer("42")
    val integer_lex_r = IntegerLiteral(INTEGER(integers_lex.head))

    assertResult(integer_lex_r)(integer(integers_lex).get)
  }

  test("some") {
    val input = Lexer("null")

    println(expression(input))

    println(input.head.toSyntaxKind)
  }
}
