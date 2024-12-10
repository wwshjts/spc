package org.syspro.spc
package parse

import org.scalatest.funsuite.AnyFunSuite
import org.syspro.spc.lexer.Lexer
import org.syspro.spc.parser.grammar.Grammar.*
import org.syspro.spc.parser.grammar.Success
import org.syspro.spc.parser.parsing_tree.*


class Definitions extends AnyFunSuite{
  test("variable") {
    val input = Lexer("val a: Integer = 1 + 2 * 3")
    val res = variable_def_stmt(input)

    println(res)
  }

  test("parameter") {
    val input = Lexer("box: Box<T>")
    val res = parameter_def(input)

    println(res)
  }

  test("type parameter") {
    val input = Lexer("cmpBox <: Box<T> & Comparable<T, U>")
    val res = type_param_def(input)

    println(res)
  }

  test("function") {
    val input = Lexer(
      """
        |override def foo(): Unit
        |   println("foo")
        |""".stripMargin)

    val res = function_def(input)

    println(res)
  }
}
