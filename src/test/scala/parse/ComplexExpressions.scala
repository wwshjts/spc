package org.syspro.spc
package parse

import org.scalatest.funsuite.AnyFunSuite
import org.syspro.spc.lexer.Lexer
import org.syspro.spc.parser.grammar.Grammar.expression
import parser.parsing_tree.*
import org.syspro.spc.parser.grammar.Success


class ComplexExpressions extends AnyFunSuite{

  test("Kolbasa1") {
    val input = Lexer("(token.end - token.start) * Term.repeated(token).length")
    val res = expression(input)

    println(res)
  }

}
