/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.smt.grammar

import z3.scala._

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 * <sort> ::= <identifier> | ( <identifier> <sort>+ )
 */
object Sort {
    val Bool = SortIdentifier(IdentifierSymbol(Symbol.Bool))
    val Int = SortIdentifier(IdentifierSymbol(Symbol.Int))
    val Real = SortIdentifier(IdentifierSymbol(Symbol.Real))
}

trait Sort extends Statement


sealed case class SortIdentifier(val identifier: Identifier)
		extends Sort {
    override def toString(): String = {
        return identifier.toString()
    }
}


sealed case class SortParametric(val identifier: Identifier, val sorts: Sort*)
		extends Sort {
	
	override def toString(): String = {
        return new StringBuilder("(")
        	.append(identifier)
        	.append(" ")
        	.append(sorts.mkString(" "))
        	.append(")")
        	.toString()
	}
}