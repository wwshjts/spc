package org.syspro.spc
package parser.parsing_tree

import syspro.tm.lexer.Token
import syspro.tm.parser.{AnySyntaxKind, SyntaxKind, SyntaxNode}


/**
 * Represents IR of spc compiler parser
 * Every IR atom should extend ParsingTree
 */
sealed trait ParsingTree extends SyntaxNode with AnySyntaxKind {
  // every Parsing tree part should satisfy SyntaxNode contract
  private def kind(): SyntaxKind = this match {
    case Leaf(kind, _) => kind
    case Branch(kind, _) => kind
  }
  private def slotCount(): Int = rank()
  private def slotIndex(index: Int): SyntaxNode = apply(index).get
  private def token(): Token = this match {
    case Leaf(_, tkn) => tkn
    case _ => null // only Leaf has a token
  }

  /**
   * Returns tree descendant by its index
   */
  def apply(index: Integer): Option[ParsingTree]

  /**
   * Amount of direct descendants of tree
   */
  def rank(): Integer
}

/**
 * Represents the branch of Parsing tree
 *
 * Corresponds to nonterminal symbol of grammar
 *
 * So it have at least one descendant
 */
case class Branch(kind: SyntaxKind, _descendants: ParsingTree*) extends ParsingTree {
  private val descendants: List[ParsingTree]  = _descendants.toList
  override def rank(): Int = descendants.size
  override def apply(index: Integer): Option[ParsingTree] = if (index < rank()) Some(descendants(index)) else None
}

/**
 * Leaf of ParsingTree
 *
 * Corresponds to Terminal of grammar
 */
case class Leaf(kind: SyntaxKind, tkn: Token) extends ParsingTree {
  override def rank(): Int = 0
  override def apply(index: Integer): Option[ParsingTree] = None
}


object Leaf {
  def BAD(tkn: Token): Leaf = Leaf(SyntaxKind.BAD, tkn)
  def INDENT(tkn: Token): Leaf = Leaf(SyntaxKind.INDENT, tkn)
  def DEDENT(tkn: Token): Leaf = Leaf(SyntaxKind.DEDENT, tkn)
  def IDENTIFIER(tkn: Token): Leaf = Leaf(SyntaxKind.BOOLEAN, tkn)
  def BOOLEAN(tkn: Token): Leaf = Leaf(SyntaxKind.BOOLEAN, tkn)
  def INTEGER(tkn: Token): Leaf = Leaf(SyntaxKind.INTEGER, tkn)
  def RUNE(tkn: Token): Leaf = Leaf(SyntaxKind.RUNE, tkn)
  def STRING(tkn: Token): Leaf = Leaf(SyntaxKind.STRING, tkn)
}