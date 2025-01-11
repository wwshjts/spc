package org.syspro.spc
package language_server.data_base_like
import parser.parsing_tree.FunctionDef

class Analyzer extends Universe with TypePackage with SemanticErrorPackage with TypeParameterPackage with VariablePackage with FunctionPackage {
  override def createFunction(functionDefinition: FunctionDef): Function = ???

  override def containsType(name: String): Boolean = ???

  override def typeAnalys: Unit = ???

  override def functionAnalys: Unit = ???
}
