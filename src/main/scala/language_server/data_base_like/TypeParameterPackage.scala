package org.syspro.spc
package language_server.data_base_like
import parser.parsing_tree.{Name, TypeParamDef, PTree}

trait TypeParameterPackage { this: Universe =>

  override final type TypeVariable = TypeVariableImpl

  final case class TypeVariableImpl(name: String, definition: PTree, typeBounds: List[TypeLike]) extends TypeVariableNode {
    override def substitute(other: TypeVariable): TypeVariable = copy(name = other.name, definition = other.definition)


    override def instantiate(arg: Type): Option[Type] = {
        val satisfiesBounds = typeBounds.forall {
          case bound: Type => arg.isSubtypeOf(bound)
          case bound: TypeVariable =>
            // TODO: actually it can be done
            val desc = s"Type argument ${arg} can't be bounded by type variable $bound"
            commitSemanticError(desc, definition.firstTerminal().token().start, definition.lastTerminal().token().end)
            false
        }

        if satisfiesBounds then Some(arg) else None
    }

    override def toFormatted(tab: Int): String =
      val title = " " * tab + s"TypeVariable: $name"
      val bounds = " " * tab + "Bounds: " + typeBounds.map(f => s"$f ").mkString
      "\n" + title + "\n" + bounds + "\n"
  }

  override def createTypeVariable(tp: TypeParamDef): TypeVariable = {
    import ExtractionUtils.*
    TypeVariableImpl(
      name = tp.name,
      definition = tp,
      typeBounds = tp.type_bound.listOfBounds.map(typeName => TypeVariableImpl(typeName.nameOfType, typeName, List.empty))
    )
  }

  override def createTypeVariable(n: Name): Option[TypeVariable] = {
    import ExtractionUtils.*
    if n.typeParams.nonEmpty then
      val description = s"There is no type bound when instantiating type parameter"
      commitSemanticError(description, n.firstTerminal().token().start, n.lastTerminal().token().end)
      None
    else
      Some(
        TypeVariableImpl(
          name = n.nameOfType,
          definition = n,
          typeBounds = List.empty
        )
      )
  }

}
