/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.smt.grammar

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 * <meta_spec_constant> ::= NUMERAL | DECIMAL | STRING
 */
abstract class MetaSpecConst(val value: Any) extends Statement {
    override def toString(): String = {
        return "" + value
    }
}

sealed case class MetaSpecIntConst(override val value: Int)
		extends MetaSpecConst(value)

sealed case class MetaSpecDoubleConst(override val value: Double)
		extends MetaSpecConst(value)

sealed case class MetaSpecStringConst(override val value: String)
		extends MetaSpecConst(value)