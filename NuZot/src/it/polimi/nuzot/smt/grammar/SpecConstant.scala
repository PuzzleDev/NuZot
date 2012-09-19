/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.smt.grammar

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 * <spec_constant> ::= <numeral> | <decimal> | <string> | true | false
 */
abstract class SpecConstant(val value: Any)
		extends Statement {
	override def toString(): String = {
	    return "" + value
	}
}

sealed case class SpecBooleanConstant(override val value: Boolean)
	extends SpecConstant(value)
	
sealed case class SpecIntConstant(override val value: Int)
	extends SpecConstant(value)

sealed case class SpecDoubleConstant(override val value: Double)
	extends SpecConstant(value)
	
sealed case class SpecStringConstant(override val value: String)
	extends SpecConstant(value)

//MR: not sure it works, but I try anyway
//    this is to allow terms to be used at temporal instants
sealed case class SpecTermConstant(override val value: Term)
    extends SpecConstant(value)
