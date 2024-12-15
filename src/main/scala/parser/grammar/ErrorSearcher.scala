package org.syspro.spc
package parser.grammar

import parser.parsing_tree.{DEDENT, INDENT, SourceText, CLASS, INTERFACE, OBJECT}

import org.syspro.spc.parser.token.SyntaxKindConverter

enum Level {
  case TYPE_DEFINITION
  case FUNCTION_DEFINITION
}

sealed trait Localizer

case class ErrorSearcher(level: Level, success: Success[SourceText]) {
  val localizer: Localizer = level match
    case Level.TYPE_DEFINITION => TypeDefinition
    case Level.FUNCTION_DEFINITION => FunctionDefinition

  def localizeError(): ErrorSearcher
}

case class TypeDefinition (input: List[Token]) extends Localizer {
  private case class IndentationInfo(indent: Option[Token], dedent: Option[Token])
  private def

  private def nextClassDefinition: Option[Token] = {

  }

  /** this function is trying to locate the body of class definition */
  private def analyzeIndentation: IndentationInfo = {
    // at this step class signature consumed by parser
    val indent =
      if (SyntaxKindConverter(input.head) == INDENT)
        Some(input.head)
      else
        None


  }
}

object FunctionDefinition extends Localizer {

}