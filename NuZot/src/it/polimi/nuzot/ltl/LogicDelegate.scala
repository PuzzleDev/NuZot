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
trait LogicDelegate extends DSLInterpreterDelegate[LTLInterpreter] {
    
    def expandTemporalBooleanOperator(
            ltl: LTLInterpreter,
            term: TemporalOperator, computed: Script): Script
            
    def expandBooleanOperator(
            ltl: LTLInterpreter,
            term: BooleanOperator, computed: Script): Script
}
