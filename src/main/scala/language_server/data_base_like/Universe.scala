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

    val typeArguments: List[TypeLike]

    def directSuperTypes: List[Type]

    def derive(projection: List[TypeLike]): Option[Type]

    def isSubtypeOf(other: Type): Boolean

    def superTypes(): Set[Type]

    def toFormatted(tab: Int): String

    override def toString: String = toFormatted(1)
  }

  extension (tl: TypeLike) {
    def toFormatted(tab: Int): String =
      tl match
        case tv: TypeVariable => tv.toFormatted(tab)
        case t: Type => t.toFormatted(tab)
  }
  abstract class TypeVariableNode {
    val name: String

    val definition: PTree

    val typeBounds: List[TypeLike]

    def substitute(other: TypeVariable): TypeVariable

    def instantiate(arg: Type): Result[Type]

    override def toString: String = toFormatted(1)
    def toFormatted(tab: Int): String

  }

  abstract class FunctionNode

  abstract class VariableNode

  abstract class SemanticErrorNode {
    val description: String
    val start: Int
    val end: Int

    val reasons: List[SemanticError]
  }

  def commitType(typeDefinition: TypeDefinition): Unit

  def commitTypes(typeDefinitions: List[TypeDefinition]): Unit

  def createFunction(functionDefinition: FunctionDef): Function

  def createTypeVariable(typeParameterDef: TypeParamDef): TypeVariable

  def createTypeVariable(name: Name): Option[TypeVariable]

  def lookUpType(name: String): Option[Type]

  def containsType(name: String): Boolean

  def commitSemanticError(description: String, start: Int, end: Int): SemanticError = commitSemanticError(description, start, end, List.empty)

  def commitSemanticError(description: String, start: Int, end: Int, reasons: List[SemanticError]): SemanticError

  def commitSemanticError(description: String, pTree: PTree): SemanticError = commitSemanticError(description, pTree, List.empty)

  def commitSemanticError(description: String, pTree: PTree, reasons: List[SemanticError]): SemanticError =
    commitSemanticError(description, pTree.firstTerminal().token().start, pTree.lastTerminal().token().end, reasons)

  def getErrors: List[SemanticError]


}