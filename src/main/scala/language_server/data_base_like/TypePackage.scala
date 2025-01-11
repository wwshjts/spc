package org.syspro.spc
package language_server.data_base_like
import parser.parsing_tree.{GenericName, IdentifierName, Name, NullName, TypeDefinition}

import scala.collection.mutable
import ExtractionUtils.*

trait TypePackage { this: Universe =>

  override type Type = TypeImpl

  sealed trait TypeImpl extends TypeNode {
    override def directSuperTypes: List[Type] = ???

    override def typeArguments: List[TypeLike] = ???

    override def lookUpTypeVariable(name: String): Option[TypeVariable] = ???

    override def derive(projection: List[TypeVariable]): Option[Type] = ???

    override def toFormatted(tab: Int): String = ???
  }

  private val actualTypes: mutable.Map[String, TypeImpl] = mutable.Map.empty

  case class RawType(override val name: String, override val definition: TypeDefinition, override val typeVariables: List[TypeVariable]) extends TypeImpl {
    // TODO: do this functionality with trait
    val localVariables = typeVariables.map(tv => tv.name -> tv).toMap
  }

  case class TypeWithSuperTypes(override val name: String, override val definition: TypeDefinition, override val typeVariables: List[TypeVariable], override val directSuperTypes: List[TypeWithSuperTypes]) extends TypeImpl {
    override def derive(projection: List[TypeVariable]): Option[TypeWithSuperTypes] = {
      if projection.length != typeVariables.length then
        val desc = s"Expected ${typeVariables.length} type parameters, given ${projection.length}"
        commitSemanticError(desc, definition.firstTerminal().token().start, definition.lastTerminal().token().end)
        None
      else
        val substitutions = typeVariables zip projection

        val newVars = mutable.ListBuffer.empty[TypeVariable]

        for ((v, s) <- substitutions) {
          newVars += v.substitute(s) // just open substitution, substitutes name, and its definition
        }

        Some(copy(typeVariables = newVars.toList))
    }

    override def toFormatted(tab: Int): String =
      val title = " " * tab + s"Type: $name"
      val st = " " * tab + "SuperTypes: \n" + directSuperTypes.map(dst => dst.toFormatted(tab * 2)).mkString
      val tv = " " * tab + "TypeVariables: \n" + typeVariables.map(tv => tv.toFormatted(tab * 2)).mkString

      "\n" + title + "\n" + st + "\n" + tv + "\n"
  }

  override def commitType(typeDefinition: TypeDefinition): Unit = {
    val rt = RawType (
      name = typeDefinition.name,
      definition = typeDefinition,
      typeVariables = typeDefinition.parameterDefinitions.map(createTypeVariable)
    )

    actualTypes += rt.name -> rt
  }

  private def updateTypeInfo(tp: TypeImpl): Unit = {
    actualTypes += tp.name -> tp
  }

  // concrete any type to complete type, or fails
  override def lookUpType(name: String): Option[TypeWithSuperTypes] = {
    if (actualTypes contains name) then
      val found = actualTypes(name) match
        case rt: RawType => concreteBases(rt)
        case tt: TypeWithSuperTypes => tt
      Some(found)
    else
      None
  }

  private def concreteBases(rt: RawType): TypeWithSuperTypes = {
    def lookUpAndCommitError(name: Name) = {
      val r = lookUpType(name.nameOfType)

      if r.isEmpty then
        commitSemanticError(s"In definition of: ${rt.name}. Can't find base class: ${name.nameOfType}", name.firstTerminal().token().start, name.lastTerminal().token().end)

      r
    }

    val symbolicBases: List[Name] = rt.definition.directBases

    // list of resolved bases
    val bases: List[(Name, TypeWithSuperTypes)] = symbolicBases.map{
      name => (name, lookUpAndCommitError(name))
    }.flatMap(t =>
      t._2 match
        case Some(value) => List((t._1, value))
        case None => List.empty
    )

    val res = mutable.ListBuffer.empty[TypeWithSuperTypes]

    // substitute typeParameters if there is
    for((n, t) <- bases) {
      t.derive(n.typeParams.map(name => createTypeVariable(name)).flatMap(_.toList)).foreach(tp => res += tp)
    }

    // after that only correctly resolved supertypes are here
    TypeWithSuperTypes(name = rt.name, definition = rt.definition, typeVariables = rt.typeVariables, directSuperTypes = res.toList)
  }

}
