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
trait EqualityDelegate extends DSLInterpreterDelegate {
    
    def expandEqualityOperator(
            ltl: LTLInterpreter,
            term: EqualityOperator, computed: Script): Script
}