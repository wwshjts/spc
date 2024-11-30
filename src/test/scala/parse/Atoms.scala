package org.syspro.spc
package parse

import org.scalatest.funsuite.AnyFunSuite
import org.syspro.spc.lexer.Lexer
import org.syspro.spc.parser.grammar.Grammar.*
import org.syspro.spc.parser.grammar.{Combinators, Parser, Success}
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

  test("tricky") {
    // MemberAccess((Invoke(MemberAccess(
    val input = Lexer("Term.repeated(token).count")
    val res = expression(input)

    val expected =
      Success(
        MemberAccess(
          Invoke(
            MemberAccess(
              IdentifierName(IDENTIFIER(input(0))),
              DOT(input(1)),
              IDENTIFIER(input(2))
            ),
            OPEN_PAREN(input(3)),
            SeparatedList(IdentifierName(IDENTIFIER(input(4)))),
            CLOSE_PAREN(input(5))
          ),
          DOT(input(6)),
          IDENTIFIER(input(7))
        ),List()
      )

    assertResult(expected)(res)
  }
}
