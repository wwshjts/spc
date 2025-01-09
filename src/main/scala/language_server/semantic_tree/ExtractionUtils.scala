package org.syspro.spc
package language_server.semantic_tree

import parser.parsing_tree.{GenericName, IdentifierName, Name, NullName, TypeBound, TypeDefinition, TypeParamDef}

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
    def listOfBounds: List[Name] =  typeBound.separatedList.dropSeparators
  }

  extension (typeParameterDefinition: TypeParamDef) {
    def name: String = typeParameterDefinition.identifier.token().toString
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
