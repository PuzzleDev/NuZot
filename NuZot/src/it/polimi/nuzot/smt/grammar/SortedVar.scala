/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.smt.grammar

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 * 
 * <sorted_var> ::= ( <symbol> <sort> )
 * 
 * TODO(m.sama): this is never used and should be removed!
 */
case class SortedVar(val symbol: Symbol, val sort: Sort) extends Statement {
    private def checkParam() = {    
	    if (symbol == null) {
		    throw new AssertionError("Symbol cannot be null.");
		}
		if (sort == null) {
		    throw new AssertionError("Symbol cannot be null.");
		}
	}
	checkParam();	
	
	override def toString(): String = {
	    return "( " + symbol.toString() + " " +sort.toString() + " )" 
	}
}