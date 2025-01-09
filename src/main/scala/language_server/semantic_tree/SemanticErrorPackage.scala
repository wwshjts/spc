package org.syspro.spc
package language_server.semantic_tree

trait SemanticErrorPackage { this: Universe =>
  override type SemanticError = SemanticErrorImpl

  case class SemanticErrorImpl(description: String, start: Int, end: Int, reasons: List[SemanticError]) extends ErrorNode {
    def appendReason(semanticError: SemanticError): SemanticError = copy(reasons = semanticError :: reasons)
  }

  override def SemanticError(description: String, start: Int, end: Int): SemanticError = {
    // TODO: add system, that analyzes
    SemanticErrorImpl(description, start, end, List.empty)
  }

  override def SemanticError(description: String, start: Int, end: Int, reasons: List[SemanticError]): SemanticError = {
    SemanticErrorImpl(description, start, end, reasons)
  }
}
