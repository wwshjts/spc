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

  test("GroupBy") {
    val input = Lexer("(a + b) * 3")
    val res = expression(input)

    val expected =
      Success(
        MULTIPLY(
          GroupBy(
            OPEN_PAREN(input(0)),
            ADD(
              IdentifierName(IDENTIFIER(input(1))),
              PLUS(input(2)),
              IdentifierName(IDENTIFIER(input(3)))
            ),
            CLOSE_PAREN(input(4))
          ),
          ASTERISK(input(5)),
          IntegerLiteral(INTEGER(input(6)))
        ),
        List()
      )

    assertResult(expected)(res)
  }

  test("member_access") {
    val input = Lexer("token.start.get_line")
    val res = expression(input)

    val expected =
      Success(
        MemberAccess(
          MemberAccess(
            IdentifierName(IDENTIFIER(input(0))),
            DOT(input(1)),
            IDENTIFIER(input(2))
          ),
          DOT(input(3)),
          IDENTIFIER(input(4))
        ),
        List()
      )

    assertResult(expected)(res)
  }

  test("index expression") {
    val input = Lexer("set[2][2 * n + 1]")
    val res = expression(input)

    val expected =
      Success(
        Index(
          Index(
            IdentifierName(IDENTIFIER(input(0))),
            OPEN_BRACKET(input(1)),
            IntegerLiteral(INTEGER(input(2))),
            CLOSE_BRACKET(input(3))
          ),
          OPEN_BRACKET(input(4)),
          ADD(
            MULTIPLY(
              IntegerLiteral(INTEGER(input(5))),
              ASTERISK(input(6)),
              IdentifierName(IDENTIFIER(input(7)))
            ),
            PLUS(input(8)),
            IntegerLiteral(INTEGER(input(9)))
          ),
          CLOSE_BRACKET(input(10))
        ),
        List()
      )

    assertResult(expected)(res)

  }

  test("invoke") {
    val input = Lexer("f(a, b, 3 + 1)")
    val res = expression(input)

    val expected =
      Success(
        Invoke(
          IdentifierName(IDENTIFIER(input(0))),
          OPEN_PAREN(input(1)),
          SeparatedList(
            IdentifierName(IDENTIFIER(input(2))),
            COMMA(input(3)),
            IdentifierName(IDENTIFIER(input(4))),
            COMMA(input(5)),
            ADD(
              IntegerLiteral(INTEGER(input(6))),
              PLUS(input(7)),
              IntegerLiteral(INTEGER(input(8))))
          ),
          CLOSE_PAREN(input(9))
      ), List()
    )


    assertResult(expected)(res)
  }

  test("tricky") {
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
