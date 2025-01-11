package org.syspro.spc
package language_server.data_base_like

import parser.parsing_tree.*

object ExtractionUtils {
  extension (name: Name) {
    def nameOfType: String = name match
      case IdentifierName(op) => op.token().toString
      case NullName(op, name) => name.token().toString
      case GenericName(i, l, separatedList, r) => i.token().toString

    def typeParams: List[Name] = name match
      case _: IdentifierName => List.empty
      case _: NullName => List.empty
      case gn: GenericName => gn.separatedList.dropSeparators
  }

  extension (typeBound: TypeBound) {
    def listOfBounds: List[Name] =
      if typeBound == null then
        List.empty
      else
        typeBound.separatedList.dropSeparators
  }

  extension (typeParameterDefinition: TypeParamDef) {
    def name: String = typeParameterDefinition.identifier.token().toString
  }

  extension (sourceText: SourceText) {
    def typeDefinitions: List[TypeDefinition] = sourceText.text.trees.map(_.asInstanceOf[TypeDefinition])
  }

  extension (typeDefinition: TypeDefinition) {
    def name: String = typeDefinition.identifier.token().toString

    /**
     * @return List of direct superClasses names
     */
    def directBases: List[Name] = {
      if typeDefinition.bound != null then
        typeDefinition.bound.listOfBounds
      else
        List.empty
    }

    def parameterDefinitions: List[TypeParamDef] = {
      if typeDefinition.params != null then
        typeDefinition.params.dropSeparators
      else
        List.empty
    }
  }

}
