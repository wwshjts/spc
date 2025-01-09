package org.syspro.spc
package language_server.semantic_tree

import parser.parsing_tree.{GenericName, IdentifierName, Name, NullName, PTree, TypeDefinition}

import scala.collection.mutable

import ExtractionUtils.*

trait TypePackage { this: Universe =>

  override type Type = TypeImpl

  case class TypeImpl(override val name: String, override val definition: TypeDefinition,
                      override val scope: List[Member], override val directBases: List[Type],
                      override val typeParameters: List[TypeLike]) extends TypeNode {
    override def allSuperClasses: List[Type] = ???
    override def modifyScope(member: Member): Type = copy(scope = member :: scope)

    def hasCompleteScope: Boolean = scope != null

    def hasCompleteDirectBases: Boolean = directBases != null

    def isComplete = hasCompleteScope && hasCompleteDirectBases

    def isIncomplete = !isComplete

    override def typeVariables: List[TypeVariable] = typeParameters.filter(_.isInstanceOf[TypeVariable]).map(_.asInstanceOf[TypeVariable])

    override def typeArguments: List[Type] = typeParameters.filter(_.isInstanceOf[Type]).map(_.asInstanceOf[Type])

    override def derive(projection: List[TypeLike]): Result[Type] = {
      if projection.length != typeArguments.length then
        val description = s"Found ${projection.length} type parameters, expected ${typeArguments.length}"
        Left(SemanticError(description, definition.firstTerminal().token().start, definition.lastTerminal().token().end))
      else
        val newVariables: mutable.ListBuffer[Result[TypeLike]] = mutable.ListBuffer.empty

        val substitutions = typeVariables zip projection

        for ((variable, projection) <- substitutions) {
          newVariables += (projection match
            case tv: TypeVariable => substituteTypeVariable(variable, tv)
            case tp: Type => instantiateTypeVariable(variable, tp))
        }

        if newVariables.exists(_.isLeft) then
          val description = s"Can't derive projection of type $name"
          val reasons: List[SemanticError] = newVariables.toList.flatMap(_.left.toSeq)
          Left(SemanticError(description, definition.firstTerminal().token().start, definition.lastTerminal().token().end, reasons))
        else
          // if substitution of variables is succeed there is no more type variables in projected type
          Right(copy(typeParameters = newVariables.toList.flatMap(_.toSeq)))
    }
  }

  def commitIncompleteType(typeDefinition: TypeDefinition)(using ctx: Context): Unit = {
    val name = typeDefinition.name
    val definition = typeDefinition

    val typeVariablesNames = typeDefinition.params.dropSeparators

    if ctx.nameTable contains name then
      val seen = ctx.nameTable(name).definition
      val reason = s"Type with name $name already seen in source file at pos ${(seen.token().start, seen.token().end)}"

      // TODO: do lesser error span
      ctx.commitError(SemanticError(reason, typeDefinition.firstTerminal().token().start, typeDefinition.lastTerminal().token().end))
    else
      ctx.commitType(
        TypeImpl(
          name = name,
          definition = typeDefinition,
          typeParameters = typeDefinition.params.dropSeparators.map(TypeVariable), // at the top-level definition only TypeVariables can be TypeParameters
          scope = null,
          directBases = null,
        )
      )
  }



  def resolveSuperType(t: Type)(using ctx: Context): Result[Type] = {

    def resolveSuperTypeSymbolic(name: Name): Result[Type] = {
      if (ctx.nameTable contains name.nameOfType) && (ctx.nameTable(name.nameOfType).hasCompleteDirectBases) then
        Right(ctx.nameTable(name.nameOfType))
      else if (ctx.nameTable contains name.nameOfType) && (!ctx.nameTable(name.nameOfType).hasCompleteDirectBases) then
        resolveSuperType(ctx.nameTable(name.nameOfType))
      else
        val description = s"Can't find definition of base type ${name.nameOfType}"
        Left(SemanticError(description, t.definition.firstTerminal().token().start, t.definition.lastTerminal().token().end))
    }


    val superTypeNames: List[Name] = t.definition.directBases

    val tmp: List[Result[Type]] = superTypeNames.map { superType =>

      def cantFindTypeParameter(name: Name): SemanticError =
        val reason = s"Can't find base type parameter ${name.nameOfType} in definition of type: ${t.name}"
        SemanticError(reason, t.definition.firstTerminal().token().start, t.definition.lastTerminal().token().end)

      val actualType = resolveSuperTypeSymbolic(superType)
      // the parameters can be same as type variable of T
      // or it can be actualType
      val params = superType.typeParams.map(pName => t.localScope.get(pName.nameOfType)).filter(_.nonEmpty).map(_.get)

      actualType.map { tp =>
        tp.derive(params)
      }
      ???

    }

    ???
  }

}
