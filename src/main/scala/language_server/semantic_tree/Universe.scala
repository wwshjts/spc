package org.syspro.spc
package language_server.semantic_tree

import parser.parsing_tree.*

import scala.collection.mutable

trait Universe {

  // Universe core
  protected type Type <: TypeNode
  protected type TypeVariable <: TypeVariableNode

  protected type Function <: FunctionNode
  protected type Variable <: VariableNode

  protected type SemanticError <: ErrorNode

  enum FunctionModifier {
    case OVERRIDE
    case ABSTRACT
    case VIRTUAL
    case NATIVE
  }

  final type Member = Function | Variable
  final type TypeLike = TypeVariable | Type
  final type Result[+A] =  Either[SemanticError, A]

  final case class Context private (nameTable: mutable.Map[String, Type], errors: mutable.ListBuffer[SemanticError]) {
    def commitError(semanticError: SemanticError): Unit =  errors += semanticError

    def commitType(t: Type): Unit = nameTable += t.name -> t

    def modifyContext(t: Type): Context = copy(nameTable = nameTable += (t.name -> t))

    def modifyContext(tps: List[Type]): Context = copy(nameTable = nameTable ++ tps.map(t => t.name -> t).toMap)
  }

  case object Context {
    def empty: Context = new Context(mutable.Map.empty, mutable.ListBuffer.empty)
  }


  val ctx: Context

  // abstract members of Universe
  // these members should be overrided by other parts of Cake
  trait TypeNode { this: Type =>
    val name: String

    val definition: TypeDefinition

    /** represents dependency by scope ownership */
    val scope: List[Member]

    /** represents dependency by inheritance relation */
    val directBases: List[Type]

    val typeParameters: List[TypeLike]

    /** returns all immediate inheritance hierarchy of this type (no order guaranteed) */
    def allSuperClasses: List[Type]

    def typeVariables: List[TypeVariable]

    def typeArguments: List[Type]

    // Type modifier methods
    def modifyScope(member: Member): Type

    def derive(projection: List[TypeLike]): Result[Type]

    // TODO: move it to TypePackage
    def localScope(using ctx: Context): Map[String, TypeLike] = ctx.nameTable.toMap ++ typeParameters.map { tp =>
      val name = tp match
        case tp: Type => tp.name
        case tv: TypeVariable => tv.name
      name -> tp
    }.toMap
  }

  trait VariableNode { this: Variable =>
    def name: String

    def definition: VariableDef

    def getType: TypeLike
  }

  trait FunctionNode { this: Function =>
    def name: String

    def definition: FunctionDef

    def returnType: Type

    def functionModifiers: List[FunctionModifier]

    def locals: List[Variable]

  }

  abstract class TypeVariableNode {
    val name: String

    val definition: PTree

    val typeBounds: List[TypeLike]
  }

  abstract class TypeArgumentNode {
    val name : String

    val definition: TypeParamDef

    val `type`: Type | TypeVariable
  }

  abstract class ErrorNode {
    val description: String
    val start: Int
    val end: Int

    val reasons: List[SemanticError]
  }

  // Abstract constructors for Nodes of Universe
  def Type(typeDefinition: TypeDefinition)(using ctx: Context): Type

  def Function(functionDefinition: FunctionDef)(using ctx: Context): Function

  def Variable(variableDefinition: VariableDef)(using ctx: Context): Variable

  def TypeVariable(typeParamDef: TypeParamDef)(using ctx: Context): TypeVariable

  // We don't actually create `TypeArguments`, so there is only way to create `TypeArgument` is instantiate  `TypeVariable`
  def instantiateTypeVariable(typeVariable: TypeVariable, t: Type)(using ctx: Context): Result[Type]

  def substituteTypeVariable(a: TypeVariable, b: TypeVariable)(using ctx: Context): Result[TypeVariable]

  def SemanticError(description: String, start: Int, end: Int): SemanticError

  def SemanticError(description: String, start: Int, end: Int, reasons: List[SemanticError]): SemanticError

  def emptyContext(): Context

}