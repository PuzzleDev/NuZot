/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.ltl.grammar

import it.polimi.nuzot.smt.grammar.UnaryOperator
import it.polimi.nuzot.smt.grammar.BinaryOperator
import it.polimi.nuzot.smt.grammar.Term
import it.polimi.nuzot.smt.grammar.TernaryOperator
import it.polimi.nuzot.smt.grammar.BooleanOperator
import it.polimi.nuzot.smt.grammar.ArithmeticOperator

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 * Defines invocation of LTL functions/operators 
 */

/**
 * Temporal operators 
 */
trait TemporalOperator extends BooleanOperator

/**
 * Aritmetic temporal operators
 */
trait ArithmeticTemporalOperator extends ArithmeticOperator

/**
 * The next operator
 * 
 * (next term)
 */
sealed case class Next(override val value: Term)
		extends UnaryOperator("next", value)
		with TemporalOperator

		
/**
 * The until operator
 * (until condition value)
 */
sealed case class Until(left: Term, right: Term)
		extends BinaryOperator("until", left, right)
		with TemporalOperator {

}
		
		
/**
 * The release operator
 * (release condition value)
 */
sealed case class Release(left: Term, right: Term)
		extends BinaryOperator("release", left, right)
		with TemporalOperator {

}


/**
 * The yesterday operator
 * (yesterday term)
 * 
 * Yesterday(K), t = 0 => false 
 */
sealed case class Yesterday(override val value: Term)
		extends UnaryOperator("yesterday", value)
		with TemporalOperator {

}


/**
 * The zeta operator
 * (zeta term)
 * 
 * Zeta(K), t = 0 => vero 
 */
sealed case class Zeta(override val value: Term)
		extends UnaryOperator("zeta", value)
		with TemporalOperator {

}


/**
 * The since operator
 * (since condition value)
 */
sealed case class Since(left: Term, right: Term)
		extends BinaryOperator("since", left, right)
		with TemporalOperator {

}


/**
 * The trigger operator
 * (trigger value condition)
 */
sealed case class Trigger(val value: Term, val condition: Term)
		extends BinaryOperator("trigger", value, condition)
		with TemporalOperator {

}
		
		
		