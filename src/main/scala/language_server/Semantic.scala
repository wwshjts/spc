package org.syspro.spc
package language_server

import parser.parsing_tree.{PTree, TypeDefinition}

// This is all about types
enum TypeKind:
  case CLASS, OBJECT, INTERFACE

sealed trait Semantic(name: String, definition: PTree)

type TypeSymbol = TypeParameter | Type
type MemberSymbol = Variable | Function

sealed abstract class Type(name: String, definition: PTree, members: List[MemberSymbol], baseTypes: List[Type]) extends Semantic(name, definition) {
  def ofMembers(members: List[MemberSymbol]): Type = this match
    case Normal(name, definition, members, baseTypes) => Normal(name, definition, members, baseTypes)
    case Generic(name, definition, members, baseTypes, type_parameters) => Generic(name, definition, members, baseTypes, type_parameters)
    case Parametrised(name, definition, members, baseTypes, type_arguments, original_definition) => Parametrised(name, definition, members, baseTypes, type_arguments, original_definition)

  def ofBaseTypes(baseTypes: List[Type]): Type = this match
    case Normal(name, definition, members, baseTypes) => Normal(name, definition, members, baseTypes)
    case Generic(name, definition, members, baseTypes, type_parameters) => Generic(name, definition, members, baseTypes, type_parameters)
    case Parametrised(name, definition, members, baseTypes, type_arguments, original_definition) => Parametrised(name, definition, members, baseTypes, type_arguments, original_definition)


}
case class Normal(name: String, definition: PTree, members: List[MemberSymbol], baseTypes: List[Type]) extends Type(name, definition, members, baseTypes)
case class Generic(name: String, definition: PTree, members: List[MemberSymbol], baseTypes: List[Type], type_parameters: List[TypeParameter]) extends Type(name, definition, members, baseTypes)
case class Parametrised(name: String, definition: PTree, members: List[MemberSymbol], baseTypes: List[Type], type_arguments: List[TypeSymbol], original_definition: Generic) extends Type(name, definition, members, baseTypes)

/**
 * @param bounds upper bound of generic argument
 * @param owner the `Generic` semantic, that firstly introduce this TypeParameter in current scope
 */
case class TypeParameter(name: String, definition: PTree, bounds: List[TypeSymbol]) extends Semantic(name, definition)
case class Variable()
case class Function()