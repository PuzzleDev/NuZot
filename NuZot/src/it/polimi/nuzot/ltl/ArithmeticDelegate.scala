/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.ltl

import it.polimi.nuzot.core.DSLInterpreterDelegate
import it.polimi.nuzot.ltl.grammar._
import it.polimi.nuzot.smt.grammar._

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 */
trait ArithmeticDelegate extends DSLInterpreterDelegate {
    
    def expandArithmeticTemporalOperator(
            ltl: LTLInterpreter,
            term: ArithmeticTemporalOperator,
            computed: Script): Script
            
    def expandArithmeticOperator(
            ltl: LTLInterpreter,
            term: ArithmeticOperator,
            computed: Script): Script
}