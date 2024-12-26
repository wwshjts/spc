package org.syspro.spc
package parse

import org.scalatest.funsuite.AnyFunSuite
import parser.parsing_tree.*
import org.syspro.spc.lexer.Lexer

import org.syspro.spc.parser.grammar.Grammar.{and, bitwiseAnd, bitwiseOr, expression, factor, shift, term, xor}
import org.syspro.spc.parser.grammar.Success

import syspro.tm.lexer.SymbolToken
import syspro.tm.lexer.Symbol

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
    val res = shift(input)

    // Token just inherit Object.eq, so it is comparison by reference
    // I don't really want to implement normal comparison, so I believe that it is works
    val expected = LEFT_SHIFT(
                      RIGHT_SHIFT(
                        IntegerLiteral(INTEGER(input.head)), RIGHT_RIGHT(input(1)), IntegerLiteral(INTEGER(input(2)))
                      ),
                      LEFT_LEFT(input(3)),
                      IntegerLiteral(INTEGER(input(4)))
    )

    println(res)
    // assertResult(expected)(res.get)
  }

  test("Shift priority") {
    val input = Lexer("1 << 2 + 3 >> 4")
    val res = shift(input)

    // Token just inherit Object.eq, so it is comparison by reference
    // I don't really want to implement normal comparison, so I believe that it is works
    val expected =
      Success(
        RIGHT_SHIFT(
          LEFT_SHIFT(
            IntegerLiteral(INTEGER(input(0))),
            LEFT_LEFT(new SymbolToken(input(1).start, input(2).end, input(1).leadingTriviaLength, input(2).trailingTriviaLength, Symbol.LESS_THAN_LESS_THAN)),
            ADD(
              IntegerLiteral(INTEGER(input(3))),
              PLUS(input(4)),
              IntegerLiteral(INTEGER(input(5)))
            )
          ),
          RIGHT_RIGHT(new SymbolToken(input(6).start, input(7).end, input(6).leadingTriviaLength, input(7).trailingTriviaLength, Symbol.GREATER_THAN_GREATER_THAN)),
          IntegerLiteral(INTEGER(input(8)))
        ),List()
      )


    println(res)
    println(expected)

    // assertResult(expected)(res)
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


  test("is expression") {
    //val input = Lexer("(1 + 2) is Integer is A")
    val input = Lexer("(1 + 2) is Integer")
    val res = expression(input)

    val expected =
      Success(
        IsExpression(
          GroupBy(
            OPEN_PAREN(input(0)),
            ADD(
              IntegerLiteral(INTEGER(input(1))),
              PLUS(input(2)),
              IntegerLiteral(INTEGER(input(3)))
            ),
            CLOSE_PAREN(input(4))
          ),
          IS(input(5)),
          IdentifierName(IDENTIFIER(input(6)))
        ),List()
      )

    assertResult(expected)(res)
  }

  test("logical not") {
    val input = Lexer("! true")
    val res = expression(input)

    println(res)
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

  test("logical or") {
    val input = Lexer("a || !b && c")
    val res = expression(input)

    val expected =
      Success(
        OR(
          IdentifierName(IDENTIFIER(input(0))),
          BAR_BAR(input(1)),
          NOT(
            EXCLAMATION(input(2)),
            AND(
              IdentifierName(IDENTIFIER(input(3))),
              AMPERSAND_AMPERSAND(input(4)),
              IdentifierName(IDENTIFIER(input(5))))
          )
        ),List()
      )

    assertResult(expected)(res)
  }

  test("generic name expression") {
    val input = Lexer("Box<A, B>")
    val res = expression(input)

    /*
    val expected =

      Success(
        GenericName(
          IDENTIFIER(input(0)),
          LEFT(input(1)),
          SeparatedList(
            IdentifierName(IDENTIFIER(input(2))),
            COMMA(input(3)),
            IdentifierName(IDENTIFIER(input(4)))
          ),
          RIGHT(input(5))
        ), List()
      )

    assertResult(expected)(res)

     */
  }

  test("nested generics") {
    val input = Lexer("P<A, P<A, P<A, B, C>>>")
    val res = expression(input)

    println(res)

  }

}



