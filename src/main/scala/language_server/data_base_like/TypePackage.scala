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

  case class TypeLink(name: String, definition: Name, typeParameters: List[TypeLike]) {
    def lookUp(): Result[CompleteType] = {
      if actualTypes contains name then
        Right(actualTypes(name))
      else
        val desc = s"Type usage refers to unknown type: $name"
        Left(commitSemanticError(desc, definition))
    }
  }
  case object TypeLink {
    def apply(nameNode: Name): TypeLink = {
      new TypeLink(
        name = nameNode.nameOfType,
        definition = nameNode,
        // some type parameters may fail their creation, but error will be committed implicitly, type with rest variables will be analyzed
        // creation will be failed in following cases Foo<T> <: Bar<T <: SomeGoodType>, so we can't bound the type parameter closure
        typeParameters = nameNode.typeParams.flatMap(parameter => createTypeVariable(parameter).toList)
      )
    }

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
                          typeVariables: List[TypeVariable], linksToSuperTypes: List[TypeLink]) extends TypeImpl with FormatableType("Complete Type") {

    def derive(link: TypeLink): Result[TypeLink] = {
      val projection = link.typeParameters // here is problem
      if projection.length != typeVariables.length then
        val desc = s"Expected ${typeVariables.length} type parameters, given ${projection.length}"
        Left(commitSemanticError(desc, definition))
      else
        val substitutions = typeVariables.zip(projection).map { (v, p) =>
          // this ugly code just hint to compiler how defer this type
          val tmp: Result[TypeLike] = p match
            case tv: TypeVariable => Right(v.substitute(tv))
            case tp: Type => v.instantiate(tp)

          tmp
        }

        val errorReasons = substitutions.flatMap(_.left.toSeq)

        if errorReasons.isEmpty then
          Right(
            TypeLink(
              name = link.name,
              definition = link.definition,
              typeParameters = substitutions.flatMap(_.toSeq)
            )
          )
        else
          val desc = s"Can't instantiate type $name"
          Left(commitSemanticError(desc, definition, errorReasons))

    }

    override def isSubtypeOf(tp: Type): Boolean = {
      val inDirectBases = linksToSuperTypes.exists(link => link.name == tp.name)
      if !inDirectBases then
        linksToSuperTypes.exists { link =>
          link.lookUp().exists(superType => superType.isSubtypeOf(tp))
        }
      else
        true
    }
    override def superTypes(): Set[Type] = ???

  }

  override def commitType(typeDefinition: TypeDefinition): Unit = {

    val rt = CompleteType (
      name = typeDefinition.name,
      definition = typeDefinition,
      // Types only can have typeVariables, clash of them will be lazy checked later, but before this type will be given to view
      typeVariables = typeDefinition.parameterDefinitions.map(createTypeVariable),
      linksToSuperTypes = typeDefinition.directBases.map(n => TypeLink(n))
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

  private def concreteBases(t: CompleteType): Result[CompleteType] = {
    /* What I need to check here
      1. Link points to existing type
      2. TypeLink and corresponding Type have the same signature
      3. TypeLink instantiation:
        3.1 Check whether link parameter have actual type
        3.2 If it has, then check types
     */
    val resolvedLinks: List[Result[TypeLink]] = t.linksToSuperTypes.map { link =>
      link.lookUp().flatMap { tp =>
        val actualParameters: List[TypeLike] = link.typeParameters.map {
          case tv: TypeVariable => actualTypes.getOrElse[TypeLike](tv.name, tv)
          case t: Type => t
        }

        tp.derive(link.copy(typeParameters = actualParameters))
      }
    }

    val errorReasons = resolvedLinks.flatMap(_.left.toSeq)

    if errorReasons.nonEmpty then
      val desc = s"In definition of ${t.name}"
      Left(desc, t.definition, errorReasons)
    else
      Right(t.copy(linksToSuperTypes = resolvedLinks.flatMap(_.toSeq)))
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
