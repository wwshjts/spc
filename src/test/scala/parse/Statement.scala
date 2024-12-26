package org.syspro.spc
package parse

import org.scalatest.funsuite.AnyFunSuite
import org.syspro.spc.Lexer
import org.syspro.spc.parser.grammar.Grammar.*
import org.syspro.spc.parser.grammar.Success
import org.syspro.spc.parser.parsing_tree.*

class Statement extends AnyFunSuite {
  test("assignment") {
    val input = Lexer("a = 1")
    val res = statement(input)
    val expected =
      Success(
        Assignment(
          IdentifierName(IDENTIFIER(input(0))),
          EQ(input(1)),
          IntegerLiteral(INTEGER(input(2)))
        ), List()
      )

    assertResult(expected)(res)
  }

  test("for") {
    val input =
      Lexer("""
        |for i in range(0, 42)
        |    print = i
        |""".stripMargin)

    val res = statement(input)

    println(res)
  }

  test("minimal for") {
    val input = Lexer("for i in range(0, 42)")

    val res = statement(input)
    // TODO:
  }

  test("minimal while") {
    val input = Lexer(
      """
        |while true
        |  doSmth = 3
        |""".stripMargin)
    val res = statement(input)

    println(input)
    println(res)
  }

  test("if stmt") {
    val input = Lexer(
      """
        |if !flag
        |  tmp = a
        |  a = b
        |  b = tmp
        |  print(a, b)
        |else
        |  flat = true
        |""".stripMargin
    )

    val res = statement(input)

    println(res)
  }
}
