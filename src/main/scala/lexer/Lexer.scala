package org.syspro.spc
package lexer

import lexer.SpcLexer

import syspro.tm.lexer.Token

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
