package org.syspro.spc
package language_server

import language_server.TypeDefinitionSemantic.Modifier
import parser.parsing_tree.*

import org.syspro.spc.language_server.FunctionSemantic.FuncMod
import syspro.tm.parser.SyntaxNode
import syspro.tm.symbols.{FunctionSymbol, MemberSymbol, SemanticSymbol, SymbolKind, TypeLikeSymbol, TypeParameterSymbol, TypeSymbol, VariableSymbol}

import java.util

/**
 * Class that hold semantic for TypeDefinition
 */
case class TypeDefinitionSemantic private (modifier: Modifier) extends syspro.tm.symbols.TypeSymbol:
  override def isAbstract: Boolean = ???

  override def baseTypes(): util.List[_ <: TypeSymbol] = ???

  override def typeArguments(): util.List[_ <: TypeLikeSymbol] = ???

  override def originalDefinition(): TypeSymbol = ???

  override def construct(typeArguments: util.List[_ <: TypeLikeSymbol]): TypeSymbol = ???

  override def members(): util.List[_ <: MemberSymbol] = ???

  override def kind(): SymbolKind = ???

  override def name(): String = ???

  override def definition(): SyntaxNode = ???


case object TypeDefinitionSemantic:
  enum Modifier:
    case CLASS, OBJECT, INTERFACE

  def getModifier(terminal: Terminal): Modifier =
    terminal match
      case _: CLASS => Modifier.CLASS
      case _: OBJECT => Modifier.OBJECT
      case _: INTERFACE => Modifier.INTERFACE
      case _ => ??? // TODO: add error handling here

  def apply(mod: Terminal): TypeDefinitionSemantic =
    new TypeDefinitionSemantic(getModifier(mod))

case class TypeParameterSemantic(bounds: util.List[_ <: TypeLikeSymbol], owner: SemanticSymbol, kind: SymbolKind, name: String, definition: SyntaxNode) extends TypeParameterSymbol

case class FunctionSemantic private (mods: Set[FunctionSemantic.FuncMod], parameters: util.List[_ <: VariableSymbol],
                                     returnType: TypeLikeSymbol, locals: util.List[_ <:VariableSymbol],
                                     owner: SemanticSymbol, kind: SymbolKind, name: String, definition: SyntaxNode) extends FunctionSymbol:

  override def isNative: Boolean = mods contains FuncMod.NATIVE

  override def isVirtual: Boolean = mods contains FuncMod.VIRTUAL

  override def isAbstract: Boolean = mods contains FuncMod.ABSTRACT

  override def isOverride: Boolean = mods contains FuncMod.OVERRIDE

case object FunctionSemantic:
  enum FuncMod:
    case NATIVE, VIRTUAL, ABSTRACT, OVERRIDE

case class VariableSemantic(`type`: TypeLikeSymbol, kind: SymbolKind, name: String, definition: SyntaxNode, owner: SemanticSymbol) extends VariableSymbol