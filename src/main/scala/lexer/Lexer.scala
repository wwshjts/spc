package org.syspro.spc
package lexer

import syspro.tm.lexer.Token
import scala.jdk.CollectionConverters.*

/**
 * Just wrapper under SpcLex
 */
object Lexer {
  def apply(s: Predef.String): List[Token] = {
    val lexer = org.sypro.spc.lexer.SpcLexer()

    lexer.lex(s).asScala.toList
  }
}
