package org.syspro.spc
package language_server

import language_server.TypeSemantic.Modifier
import parser.parsing_tree.*

import org.syspro.spc.language_server.FunctionSemantic.FuncMod
import syspro.tm.parser.SyntaxNode
import syspro.tm.symbols.{FunctionSymbol, MemberSymbol, SemanticSymbol, SymbolKind, TypeLikeSymbol, TypeParameterSymbol, TypeSymbol, VariableSymbol}

import java.util

/**
 * Class that hold semantic for TypeDefinition
 */
case class TypeSemantic (modifier: Modifier, baseTypes: util.List[TypeSymbol], typeArguments: util.List[TypeLikeSymbol], originalDefinition: syspro.tm.symbols.TypeSymbol,
                                members: util.List[MemberSymbol], kind: SymbolKind, name: String, definition: SyntaxNode) extends syspro.tm.symbols.TypeSymbol:
  override def isAbstract: Boolean = ???

  override def construct(typeArguments: util.List[_ <: TypeLikeSymbol]): TypeSymbol = ???


case object TypeSemantic:
  enum Modifier:
    case CLASS, OBJECT, INTERFACE

  def getModifier(terminal: Terminal): Modifier =
    terminal match
      case _: CLASS => Modifier.CLASS
      case _: OBJECT => Modifier.OBJECT
      case _: INTERFACE => Modifier.INTERFACE
      case _ => ??? // TODO: add error handling here

  /*
  def apply(mod: Terminal): TypeDefinitionSemantic =
    new TypeDefinitionSemantic(getModifier(mod))

   */

case class TypeParameterSemantic(bounds: util.List[_ <: TypeLikeSymbol], owner: SemanticSymbol, kind: SymbolKind, name: String, definition: SyntaxNode) extends TypeParameterSymbol

case class FunctionSemantic (mods: Set[FunctionSemantic.FuncMod], parameters: util.List[VariableSymbol],
                                     returnType: TypeLikeSymbol, locals: util.List[VariableSymbol],
                                     owner: SemanticSymbol, kind: SymbolKind, name: String, definition: SyntaxNode) extends FunctionSymbol:

  override def isNative: Boolean = mods contains FuncMod.NATIVE

  override def isVirtual: Boolean = mods contains FuncMod.VIRTUAL

  override def isAbstract: Boolean = mods contains FuncMod.ABSTRACT

  override def isOverride: Boolean = mods contains FuncMod.OVERRIDE

case object FunctionSemantic:
  enum FuncMod:
    case NATIVE, VIRTUAL, ABSTRACT, OVERRIDE

case class VariableSemantic(`type`: TypeLikeSymbol, kind: SymbolKind, name: String, definition: SyntaxNode, owner: SemanticSymbol) extends VariableSymbol