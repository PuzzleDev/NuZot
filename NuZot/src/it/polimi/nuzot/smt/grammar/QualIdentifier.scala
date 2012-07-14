/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.smt.grammar

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 * <qual_identifier> ::= <identifier> | ( as <identifier> <sort> )
 */
abstract class QualIdentifier(val identifier: Identifier)
		extends Statement

sealed case class QualIdentifierIdentifier(
        override val identifier: Identifier)
		extends QualIdentifier(identifier) {
    override def toString(): String = {
        return identifier.toString()
    }
}

case class QualIdentifierAs(
        override val identifier: Identifier, val sort: Sort)
		extends QualIdentifier(identifier) {
	
	override def toString(): String = {
	    return "( as " + identifier + " " + sort + " )"
	}
}