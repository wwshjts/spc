package org.syspro.spc
package language_server.data_base_like

trait FunctionPackage { this: Universe =>
  override type Function = FunctionImpl
  case class FunctionImpl() extends FunctionNode
}
