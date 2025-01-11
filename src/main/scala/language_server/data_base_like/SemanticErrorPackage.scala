package org.syspro.spc
package language_server.data_base_like

trait SemanticErrorPackage { this: Universe =>
  override type SemanticError = SemanticErrorImpl

  case class SemanticErrorImpl(description: String, start: Int, end: Int, reasons: List[SemanticError]) extends SemanticErrorNode {
    def appendReason(semanticError: SemanticError): SemanticError = copy(reasons = semanticError :: reasons)
  }

  override def SemanticError(description: String, start: Int, end: Int, reasons: List[SemanticError]): SemanticError = {
    SemanticErrorImpl(description, start, end, reasons)
  }
}
