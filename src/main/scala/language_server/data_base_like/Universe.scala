package org.syspro.spc
package language_server.data_base_like

import parser.parsing_tree.{FunctionDef, PTree, TypeDefinition, TypeParamDef, Name}

// represents Type container, actually can have many realisations
trait Universe {
  protected type Type <: TypeNode
  protected type TypeVariable <: TypeVariableNode
  final type TypeLike = Type | TypeVariable

  protected type Function <: FunctionNode
  protected type Variable <: VariableNode

  protected type SemanticError <: SemanticErrorNode
  final type Result[+A] = Either[SemanticError, A]

  final type Member = Variable | Function

  abstract class TypeNode {
    val name: String

    val definition: TypeDefinition

    val typeVariables: List[TypeVariable]

    def directSuperTypes: List[Type]

    def typeArguments: List[TypeLike]

    def derive(projection: List[TypeLike]): Result[Type]

    def lookUpTypeVariable(name: String): Option[TypeVariable]
  }

  abstract class TypeVariableNode {
    val name: String

    val definition: PTree

    val typeBounds: List[TypeLike]

    def substitute(other: TypeVariable): TypeVariable
  }

  sealed abstract class FunctionNode

  abstract class SemanticErrorNode {
    val description: String
    val start: Int
    val end: Int

    val reasons: List[SemanticError]
  }

  def commitType(typeDefinition: TypeDefinition): Unit

  def createFunction(functionDefinition: FunctionDef): Function

  def createTypeVariable(typeParameterDef: TypeParamDef): TypeVariable

  def createTypeVariable(name: Name): Result[TypeVariable]

  def lookUpType(name: String): Result[Type]

  def containsType(name: String): Boolean

  def commitSemanticError(description: String, start: Int, end: Int): SemanticError = commitSemanticError(description, start, end, List.empty)

  def commitSemanticError(description: String, start: Int, end: Int, reasons: List[SemanticError]): SemanticError

  def typeAnalys: Unit

  def functionAnalys: Unit


}