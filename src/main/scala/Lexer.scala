package org.syspro.spc

import syspro.tm.lexer.Token

import lexer.SpcLexer

import scala.jdk.CollectionConverters.*

/**
 * Just wrapper under SpcLex
 */
object Lexer {
  def apply(s: Predef.String): List[Token] = {
    val lexer = SpcLexer()

    lexer.lex(s).asScala.toList
  }
}
