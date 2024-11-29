package org.syspro.spc
package parse

import org.scalatest.funsuite.AnyFunSuite
import org.syspro.spc.lexer.Lexer
import parser.parsing_tree.*

import org.syspro.spc.parser.grammar.Grammar.{and, expression, shift}
import org.syspro.spc.parser.grammar.Success

// import org.syspro.spc.parser.parsing_tree.{INTEGER, IntegerLiteral, LEFT_SHIFT, RIGHT_SHIFT, RIGHT_RIGHT}



class Expression extends AnyFunSuite {
  test("shifts") {
    val input = Lexer("1 >> 2 << 2")
    
    val expected = LEFT_SHIFT(
                      RIGHT_SHIFT(
                        IntegerLiteral(INTEGER(input.head)), RIGHT_RIGHT(input(1)), IntegerLiteral(INTEGER(input(2)))
                      ),
                      LEFT_LEFT(input(3)),
                      IntegerLiteral(INTEGER(input(4)))
    )

    val res = shift(input)
    assertResult(expected)(res.get)
  }

  test("Shift priority") {
    val input = Lexer("1 << 2 + 3 >> 4")
    val res = shift(input)

    val expected =
      Success(
        RIGHT_SHIFT(
          LEFT_SHIFT(
            IntegerLiteral(INTEGER(input(0))),
            LEFT_LEFT(input(1)),
            ADD(
              IntegerLiteral(INTEGER(input(2))),PLUS(input(3)),IntegerLiteral(INTEGER(input(4))))),
          RIGHT_RIGHT(input(5)),
          IntegerLiteral(INTEGER(input(6)))),
        List())

    assertResult(expected)(res)
  }

  test("logical and") {
    val input = Lexer("1 && 2")
    val res = and(input)

    val expected =
      Success(
        AND(
          IntegerLiteral(INTEGER(input(0))),
          AMPERSAND_AMPERSAND(input(1)),
          IntegerLiteral(INTEGER(input(2)))
        ),
      List())

    assertResult(expected)(res)
  }

  test("logical end priority") {
    val input = Lexer("1 && 2 * 3")
    val res = and(input)

    val expected =
      Success(
        AND(
          IntegerLiteral(INTEGER(input.head)),
          AMPERSAND_AMPERSAND(input(1)),
          MULTIPLY(
            IntegerLiteral(INTEGER(input(2))),
            ASTERISK(input(3)),
            IntegerLiteral(INTEGER(input(4))))
        ),
      List())

    assertResult(expected)(res)
  }
}

