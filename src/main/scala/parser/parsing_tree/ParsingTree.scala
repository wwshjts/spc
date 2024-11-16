package org.syspro.spc
package parser.parsing_tree

import syspro.tm.lexer.Token
import syspro.tm.parser.{AnySyntaxKind, SyntaxKind, SyntaxNode}


/**
 * Represents IR of spc compiler parser
 * Every IR atom should extend ParsingTree
 */
sealed trait ParsingTree(syntaxKind: AnySyntaxKind) extends SyntaxNode with AnySyntaxKind {
  // every Parsing tree part should satisfy SyntaxNode contract
  override def kind(): AnySyntaxKind = syntaxKind
  override def slotCount(): Int = rank()
  override def slot(index: Int): SyntaxNode = apply(index).get
  override def token(): Token = null

  /**
   * Returns tree descendant by its index
   */
  def apply(index: Int): Option[ParsingTree]

  /**
   * Amount of direct descendants of tree
   */
  def rank(): Int
}

/**
 * Represents the branch of Parsing tree
 *
 * Corresponds to nonterminal symbol of grammar
 *
 * So it have at least one descendant
 */
case class Branch(syntaxKind: AnySyntaxKind, _descendants: ParsingTree*) extends ParsingTree(syntaxKind) {
  private val descendants: List[ParsingTree]  = _descendants.toList
  override def rank(): Int = descendants.size
  override def apply(index: Int): Option[ParsingTree] = if (index < rank()) Some(descendants(index)) else None
}

/**
 * Leaf of ParsingTree
 *
 * Corresponds to Terminal of grammar
 */
case class Leaf(syntaxKind: AnySyntaxKind, tkn: Token) extends ParsingTree(syntaxKind) {
  override def rank(): Int = 0
  override def apply(index: Int): Option[ParsingTree] = None
  override def token(): Token = tkn
}


object Leaf {
  def BAD(tkn: Token): Leaf         = new Leaf(SyntaxKind.BAD, tkn)
  def INDENT(tkn: Token): Leaf      = new Leaf(SyntaxKind.INDENT, tkn)
  def DEDENT(tkn: Token): Leaf      = new Leaf(SyntaxKind.DEDENT, tkn)
  def IDENTIFIER(tkn: Token): Leaf  = new Leaf(SyntaxKind.BOOLEAN, tkn)
  def BOOLEAN(tkn: Token): Leaf     = new Leaf(SyntaxKind.BOOLEAN, tkn)
  def INTEGER(tkn: Token): Leaf     = new Leaf(SyntaxKind.INTEGER, tkn)
  def RUNE(tkn: Token): Leaf        = new Leaf(SyntaxKind.RUNE, tkn)
  def STRING(tkn: Token): Leaf      = new Leaf(SyntaxKind.STRING, tkn)

  def apply(syntaxKind: SyntaxKind, tkn: Token): Leaf = syntaxKind match {
    case SyntaxKind.BAD         => BAD(tkn)
    case SyntaxKind.INDENT      => INDENT(tkn)
    case SyntaxKind.DEDENT      => DEDENT(tkn)
    case SyntaxKind.IDENTIFIER  => IDENTIFIER(tkn)
    case SyntaxKind.BOOLEAN     => BOOLEAN(tkn)
    case SyntaxKind.INTEGER     => INTEGER(tkn)
    case SyntaxKind.RUNE        => RUNE(tkn)
    case SyntaxKind.STRING      => STRING(tkn)
  }
}