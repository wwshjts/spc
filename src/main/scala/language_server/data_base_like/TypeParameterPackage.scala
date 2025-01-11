package org.syspro.spc
package language_server.data_base_like
import parser.parsing_tree.{Name, TypeParamDef, PTree}

trait TypeParameterPackage { this: Universe =>

  override final type TypeVariable = TypeVariableImpl

  final case class TypeVariableImpl(name: String, definition: PTree, typeBounds: List[TypeLike]) extends TypeVariableNode {
    override def substitute(other: TypeVariable): TypeVariable = copy(name = other.name, definition = other.definition)
  }

  override def TypeVariable(tp: TypeParamDef): TypeVariable = {
    import ExtractionUtils.*
    TypeVariableImpl(
      name = tp.name,
      definition = tp,
      typeBounds = tp.type_bound.listOfBounds.map(typeName => TypeVariableImpl(typeName.nameOfType, typeName, null))
    )
  }

  override def TypeVariable(n: Name): Result[TypeVariable] = {
    import ExtractionUtils.*
    if n.typeParams.nonEmpty then
      val description = s"There is no type bound when instantiating type parameter"
      Left(commitSemanticError(description, n.firstTerminal().token().start, n.lastTerminal().token().end))
    else
      Right(
        TypeVariableImpl(
          name = n.nameOfType,
          definition = n,
          typeBounds = List.empty
        )
      )
  }

}
