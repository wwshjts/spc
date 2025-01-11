package org.syspro.spc
package language_server.data_base_like
import parser.parsing_tree.{GenericName, IdentifierName, Name, NullName, TypeDefinition}

import scala.collection.mutable
import ExtractionUtils.*

trait TypePackage { this: Universe =>

  override type Type = TypeImpl

  private val actualTypes: mutable.Map[String, TypeImpl] = mutable.Map.empty

  // I need more MixIns!!!!!

  trait FormatableType(title: String) { this: Type =>
    override def toFormatted(tab: Int): String =
      val t = " " * tab + s"$title: $name"
      val st = " " * tab + "SuperTypes: \n" + directSuperTypes.map(dst => dst.toFormatted(tab * 2)).mkString
      val tv = " " * tab + "TypeVariables: \n" + typeArguments.map(tv => tv.toFormatted(tab * 2)).mkString

      "\n" + t + "\n" + st + "\n" + tv + "\n"
  }

  sealed trait TypeImpl extends TypeNode {
    override val name: String = ???
    override val definition: TypeDefinition = ???
    override val typeArguments: List[TypeLike] = ???

    override def directSuperTypes: List[Type] = ???

    override def derive(projection: List[TypeLike]): Option[Type] = ???

    override def isSubtypeOf(other: Type): Boolean = ???

    override def superTypes(): Set[Type] = ???

    override def toFormatted(tab: Int): String = ???
  }
  case class RawType(override val name: String, override val definition: TypeDefinition, typeVariables: List[TypeVariable]) extends TypeImpl {
    override val typeArguments: List[TypeLike] = typeVariables
  }

  case class TypeWithSuperTypes(override val name: String, override val definition: TypeDefinition, override val directSuperTypes: List[TypeWithSuperTypes], typeVariables: List[TypeVariable]) extends TypeImpl with FormatableType("Type with resolved supertypes") {
    override val typeArguments: List[TypeLike] = typeVariables
    def substituteVariables(projection: List[TypeVariable]): Option[TypeWithSuperTypes] = {
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
  }

  case class CompleteType(override val name: String, override val definition: TypeDefinition,
                                override val typeArguments: List[TypeLike], override val directSuperTypes: List[CompleteType]) extends TypeImpl with FormatableType("Complete Type") {

    override def derive(projection: List[TypeLike]): Option[CompleteType] = {
      if projection.length != typeArguments.length then
        val desc = s"Expected ${typeArguments.length} type parameters, given ${projection.length}"
        commitSemanticError(desc, definition.firstTerminal().token().start, definition.lastTerminal().token().end)
        None
      else
        val substitutions = typeArguments zip projection

        val afterSubs = substitutions.map{ (arg, s) =>
          arg match
            case tv: TypeVariable =>
              val tmp: Option[TypeLike] = s match
                case s: Type => tv.instantiate(s)
                case s: TypeVariable => Some(tv.substitute(s))

              tmp
            case t: Type =>
              val desc = s"In instantiation of Type $name: can't substitute actual type argument ${t.name}"
              commitSemanticError(desc, definition.firstTerminal().token().start, definition.lastTerminal().token().end)
              None
        }.flatMap(t => t.toList)

        if afterSubs.length == typeArguments.length then Some(copy(typeArguments = afterSubs)) else None
    }


    override def isSubtypeOf(tp: Type): Boolean = superTypes() contains tp
    override def superTypes(): Set[Type] = directSuperTypes.toSet[Type].flatMap(_.superTypes())

  }

  override def commitType(typeDefinition: TypeDefinition): Unit = {
    val rt = RawType (
      name = typeDefinition.name,
      definition = typeDefinition,
      typeVariables = typeDefinition.parameterDefinitions.map(createTypeVariable)
    )

    actualTypes += rt.name -> rt
  }

  override def commitTypes(typeDefinitions: List[TypeDefinition]): Unit = {
    for (td <- typeDefinitions) {
      commitType(td)
    }
  }

  private def superTypeAnalysis(): Unit = {
    val tps = actualTypes.toList.map(_._2)
    ???
  }

  // concrete any type to complete type, or fails
  def analyzeHierarchy(name: String): Option[TypeWithSuperTypes] = {
    if (actualTypes contains name) then
      val found = actualTypes(name) match
        case rt: RawType => concreteBases(rt)
        case tt: TypeWithSuperTypes => tt
        case ct: CompleteType => ct
      Some(found)
    else
      None
  }

  private def concreteBases(rt: RawType): TypeWithSuperTypes = {
    def analyzeAndCommitError(name: Name) = {
      val r = analyzeHierarchy(name.nameOfType)
      if r.isEmpty then commitSemanticError(s"In definition of: ${rt.name}. Can't find base class: ${name.nameOfType}", name.firstTerminal().token().start, name.lastTerminal().token().end)
      r
    }

    val symbolicBases: List[Name] = rt.definition.directBases

    // list of resolved bases
    val bases: List[(Name, TypeWithSuperTypes)] = symbolicBases.map{
      name => (name, analyzeAndCommitError(name))
    }.flatMap(t =>
      t._2 match
        case Some(value) => List((t._1, value))
        case None => List.empty
    )

    val res = mutable.ListBuffer.empty[TypeWithSuperTypes]

    // substitute typeParameters if there is
    for((n, t) <- bases) {
      t.substituteVariables(n.typeParams.map(name => createTypeVariable(name)).flatMap(_.toList)).foreach(tp => res += tp)
    }

    // after that only correctly resolved supertypes are here
    TypeWithSuperTypes(name = rt.name, definition = rt.definition, typeVariables = rt.typeVariables, directSuperTypes = res.toList)
  }


  private def analyzeTypeParameters(name: String): Option[CompleteType] = {
    if (actualTypes contains name) then
      val found = actualTypes(name) match
        case rt: RawType => ???
        case tt: TypeWithSuperTypes => concreteTypeParameters(tt)
        case ct: CompleteType => ct
      Some(found)
    else
      None
  }
  private def concreteTypeParameters(tt: TypeWithSuperTypes): CompleteType = {
    // if type is instance of TypeWithSuperTypes it means that it have only type variables
    val actualTypeParameters = tt.typeVariables.map(tv => tv.name -> tv).toMap

    // 2. see if bases is instantiating example: class Foo <: Bar<Int>; interface Bar<T>
    val superTypes = tt.directSuperTypes

    /*
    val superTypes = tt.directSuperTypes.flatMap { superType =>
      superType.derive(superType.typeVariables.map(tv => analyzeTypeParameters(tv.name).getOrElse(tv)))
    }
     */

    // 3. the result will contain only successfully instantiated superTypes
    CompleteType(
      name = tt.name,
      definition = tt.definition,
      typeArguments = tt.typeArguments,
      directSuperTypes = superTypes
    )
  }


  def typeParametersAnalysis(): Unit = {
    val types = actualTypes.toList.map(_._2)

    for (t <- types) {
      t
    }

  }

}
