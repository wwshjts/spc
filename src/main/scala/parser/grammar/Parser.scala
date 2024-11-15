package org.syspro.spc
package parser.grammar

import syspro.tm.lexer.Token

/**
 * A parser for things
 *
 * Is a function from list of syspro.tm.lexer.Token
 *
 * To lists of pairs
 *
 * of things and syspro.tm.lexer.Token
 * @tparam A
 */
trait Parser[A] extends (List[Token] => Option[(A, List[Token])]){

  def apply(input: List[Token]): Option[(A, List[Token])]

  // TODO: make error handling
}