package org.syspro.spc
package language_server.data_base_like

import scala.collection.mutable
trait SemanticErrorPackage { this: Universe =>
  override type SemanticError = SemanticErrorImpl

  private val errors: mutable.Set[SemanticError] = mutable.Set.empty

  case class SemanticErrorImpl(description: String, start: Int, end: Int, reasons: List[SemanticError]) extends SemanticErrorNode {
    def appendReason(semanticError: SemanticError): SemanticError = copy(reasons = semanticError :: reasons)
  }

  override def commitSemanticError(description: String, start: Int, end: Int, reasons: List[SemanticError]): SemanticError =
    val s = SemanticErrorImpl(description, start, end, reasons)
    errors += s
    s

  override def getErrors: List[SemanticError] = errors.toList
}
