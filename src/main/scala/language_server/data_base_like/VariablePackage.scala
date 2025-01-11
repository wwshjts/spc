package org.syspro.spc
package language_server.data_base_like

trait VariablePackage { this: Universe =>
  override type Variable = VariableImpl

  case class VariableImpl() extends VariableNode
}
