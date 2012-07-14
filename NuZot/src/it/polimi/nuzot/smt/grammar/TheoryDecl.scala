/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.smt.grammar

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 * <theory_decl> ::== ( theory <symbol> <theory_attribute>+ )
 */
case class TheoryDecl(val symbol: Symbol, val attributes: List[TheoryAttribute])
		extends Statement {

    override def toString(): String = {
        return new StringBuilder("( theory ")
        		.append(symbol)
        		.append(attributes.mkString(" "))
        		.append(" )")
        		.toString()
    }
}