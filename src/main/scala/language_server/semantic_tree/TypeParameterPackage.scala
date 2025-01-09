package org.syspro.spc
package language_server.semantic_tree
import parser.parsing_tree.{PTree, TypeParamDef}

trait TypeParameterPackage { this: Universe =>

  override final type TypeVariable = TypeVariableImpl
  final case class TypeVariableImpl(name: String, definition: PTree, typeBounds: List[TypeLike]) extends TypeVariableNode

  override def TypeVariable(tp: TypeParamDef)(using ctx: Context): TypeVariable = {
    import ExtractionUtils.*
    TypeVariableImpl(
      name = tp.name,
      definition = tp,
      typeBounds = tp.type_bound.listOfBounds.map(typeName => TypeVariableImpl(typeName.nameOfType, typeName, null))
    )
  }

  override def instantiateTypeVariable(typeVariable: TypeVariable, t: Type)(using ctx: Context): TypeLike = ???

}
