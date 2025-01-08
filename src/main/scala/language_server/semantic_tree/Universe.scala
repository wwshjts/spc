package org.syspro.spc
package language_server.semantic_tree

import parser.parsing_tree.*

trait Universe {

  // Universe core
  protected type Type <: TypeNode
  protected type Function <: FunctionNode
  protected type Variable <: VariableNode

  enum FunctionModifier {
    case OVERRIDE
    case ABSTRACT
    case VIRTUAL
    case NATIVE
  }

  final type Member = Function | Variable

  /** Represents usage of `Type` as type parameter */
  final case class TypeBound(t: Type, bounds: List[Type])

  // abstract members of Universe
  // these members should be overrided by other parts of Cake
  trait TypeNode { this: Type =>
    def name: String

    def definition: PTree

    /** represents dependency by scope ownership */
    def scope: List[Member]

    /** represents dependency by inheritance relation */
    def directBases: List[Type]

    /** returns all immediate inheritance hierarchy of this type (no order guaranteed) */
    def allSuperClasses: List[Type]

    /** represents dependency by type parameters */
    def typeParameters: List[TypeBound]
  }

  trait VariableNode { this: Variable =>
    def name: String

    def definition: PTree

    def getType: Type
  }

  trait FunctionNode { this: Function =>
    def name: String

    def definition: PTree

    def returnType: Type

    def functionModifiers: List[FunctionModifier]

    def locals: List[Variable]

  }


  // Abstract constructors for Nodes of Universe
  def Type(typeDefinition: TypeDefinition): Type

  def Function(functionDefinition: FunctionDef): Function

  def Variable(variableDefinition: VariableDef): Variable
}