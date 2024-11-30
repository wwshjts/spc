package org.syspro.spc
package parse

import org.scalatest.funsuite.AnyFunSuite
import org.syspro.spc.lexer.Lexer
import parser.parsing_tree.*

import org.syspro.spc.parser.grammar.Grammar.{and, bitwiseAnd, bitwiseOr, expression, factor, shift, term, xor}
import org.syspro.spc.parser.grammar.Success

// import org.syspro.spc.parser.parsing_tree.{INTEGER, IntegerLiteral, LEFT_SHIFT, RIGHT_SHIFT, RIGHT_RIGHT}



class Expression extends AnyFunSuite {

  test("factors are left associative") {
    val input = Lexer("1 * 2 / 3 * 4")
    val res = factor(input)

    val expected =
      Success(
        MULTIPLY(
          DIV(
            MULTIPLY(
              IntegerLiteral(INTEGER(input(0))),
              ASTERISK(input(1)),
              IntegerLiteral(INTEGER(input(2)))
            ),
            SLASH(input(3)),
            IntegerLiteral(INTEGER(input(4)))
          ),
          ASTERISK(input(5)),
          IntegerLiteral(INTEGER(input(6)))
        ), List()
      )

    assertResult(expected)(res)
  }

  test("term") {
    val input = Lexer("1 + -2 * 3 - 4")
    val res = term(input)

    val expected =
      Success(
        SUBTRACT(
          ADD(
            IntegerLiteral(INTEGER(input(0))),
            PLUS(input(1)),
            MULTIPLY(
              Negate(
                MINUS(input(2)),
                IntegerLiteral(INTEGER(input(3)))
              ),
              ASTERISK(input(4)),
              IntegerLiteral(INTEGER(input(5))))
          ),
          MINUS(input(6)),
          IntegerLiteral(INTEGER(input(7)))
        ),List()
      )

    assertResult(expected)(res)
  }

  test("factor should also match expressions with lower priority") {
    val input = Lexer("-1")
    val res = factor(input)

    val expected =
      Success(
        Negate(
          MINUS(input(0)),
          IntegerLiteral(INTEGER(input(1)))
        ), List()
      )
  }


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

  test("bitwise and") {
    val input = Lexer("1 & 2")
    val res = bitwiseAnd(input)

    val expected =
      Success(
        BitwiseAnd(
          IntegerLiteral(INTEGER(input.head)),
          AMPERSAND(input(1)),
          IntegerLiteral(INTEGER(input(2)))
        ),
      List())

    assertResult(expected)(res)
  }

  test("bitwise and priority") {
    val input = Lexer("1 & 2 - 3")
    val res = bitwiseAnd(input)

    val expected =
      Success(
        BitwiseAnd(
          IntegerLiteral(INTEGER(input.head)),
          AMPERSAND(input(1)),
          SUBTRACT(
            IntegerLiteral(INTEGER(input(2))),
            MINUS(input(3)),
            IntegerLiteral(INTEGER(input(4)))
          )
        ),List())

    assertResult(expected)(res)
  }

  test("xor") {
    val input = Lexer("1 & 2 ^ 3")
    val res = xor(input)

    val expected =
      Success(
        Xor(
          BitwiseAnd(
            IntegerLiteral(INTEGER(input.head)),
            AMPERSAND(input(1)),
            IntegerLiteral(INTEGER(input(2)))
          ),
          CARET(input(3)),
          IntegerLiteral(INTEGER(input(4)))
        ),List()
      )

    assertResult(expected)(res)
  }

  test("or") {
    val input = Lexer("1 & 2 | 3 ^ 4")
    val res = bitwiseOr(input)

    val expected =
      Success(
        BitwiseOr(
          BitwiseAnd(
            IntegerLiteral(INTEGER(input.head)),
            AMPERSAND(input(1)),
            IntegerLiteral(INTEGER(input(2)))
          ),
          BAR(input(3)),
          Xor(
            IntegerLiteral(INTEGER(input(4))),
            CARET(input(5)),
            IntegerLiteral(INTEGER(input(6)))
          )
        ),List()
      )

    assertResult(expected)(res)
  }

  test("less greater") {
    val input = Lexer("1 < 2 > 3")
    val res = expression(input)

    val expected =
      Success(
        GreaterThan(
          LessThan(
            IntegerLiteral(INTEGER(input(0))),
            LEFT(input(1)),
            IntegerLiteral(INTEGER(input(2)))
          ),
          RIGHT(input(3)),
          IntegerLiteral(INTEGER(input(4)))
        ),List()
      )

    assertResult(expected)(res)
  }

  test("equals") {
    val input = Lexer("1 == 2 != 3")
    val res = expression(input)

    val expected =
      Success(
        NEqual(
          Equal(
            IntegerLiteral(INTEGER(input(0))),
            EQ_EQ(input(1)),
            IntegerLiteral(INTEGER(input(2)))
          ),
          NEQ(input(3)),
          IntegerLiteral(INTEGER(input(4)))
        ),List()
      )

    assertResult(expected)(res)

  }

  test("<= >=") {
    val input = Lexer("1 <= 2 >= 3")
    val res = expression(input)

    val expected =
      Success(
        GreaterOrEq(
          LessOrEq(
            IntegerLiteral(INTEGER(input(0))),
            LEFT_EQ(input(1)),
            IntegerLiteral(INTEGER(input(2)))
          ),
          RIGHT_EQ(input(3)),
          IntegerLiteral(INTEGER(input(4)))
        ),List()
      )

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

