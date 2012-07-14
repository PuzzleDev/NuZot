/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.smt.grammar

import scala.collection.immutable.Queue

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 * <identifier> ::== <symbol> | ( _<symbol> <numeral>+ ) 
 * 
 * XXX(m.sama): This complicates the grammar for no reason
 */
abstract class Identifier(val symbol: Symbol)
	extends Statement


case class IdentifierSymbol(override val symbol: Symbol)
		extends Identifier(symbol) {
    
	override def toString(): String = {
        return symbol.toString()
	}
}


case class IdentifierSymbolNum(override val symbol: Symbol, val values: List[AnyVal])
		extends Identifier(symbol) {
    
	override def toString(): String = {
        return new StringBuilder("( _")
        	.append(symbol)
        	.append(" ")
        	.append(values.mkString(" "))
        	.append(" )")
        	.toString()
	}
}
		