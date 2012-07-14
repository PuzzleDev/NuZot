/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.ltl

import it.polimi.nuzot.smt.grammar._

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 */
class DefaultEqualityDelegate extends EqualityDelegate {

    override def generatePreconditions(ltl: LTLInterpreter): Script = {
	    return new Script()
    }
    
    override def expandEqualityOperator(
            ltl: LTLInterpreter,
            term: EqualityOperator, computed: Script): Script = {
        val supportFz = ltl.generateTemporalSupportFzName(term)
        var script = computed
        
        // Same as aritmetic
        // 0 <= i <= k -> zot-pX(i) <-> (AOP x(i) y(i))
        for (i <- 0 until ltl.temporalExt + 2) {
            script = script :+ CommandAssert(
                    IFF(
                        Term.call(Symbol(supportFz), TermConst(ltl.const(i))),
                    	ltl.expandTemporalFunctionsAtTime(term, ltl.const(i))
                		)	
            		)
        }
        
        script = ltl.expandSubformula(term.value1, script)
        script = ltl.expandSubformula(term.value2, script)
        return script
    }
}