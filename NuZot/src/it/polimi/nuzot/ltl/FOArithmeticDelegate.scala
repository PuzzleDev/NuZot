/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.ltl

import it.polimi.nuzot.ltl.grammar._
import it.polimi.nuzot.smt.grammar._

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 */
class FOArithmeticDelegate extends ArithmeticDelegate {

    override def generatePreconditions(ltl: LTLInterpreter): Script = {
	    return new Script()
    }
    
    override def expandArithmeticTemporalOperator(
            ltl: LTLInterpreter,
            term: ArithmeticTemporalOperator,
            computed: Script): Script = {
        throw new IllegalArgumentException(term + 
                        " not implemented yet")
    }
            
    override def expandArithmeticOperator(
            ltl: LTLInterpreter,
            term: ArithmeticOperator,
            computed: Script): Script = {
        val supportFz = ltl.generateTemporalSupportFzName(term)
        var script = computed
        
        // 0 <= i <= k -> zot-pX(i) <-> (AOP x(i) y(i))
        for (i <- 0 until ltl.temporalExt + 2) {
            script = script :+ CommandAssert(
                    IFF(
                        Term.call(Symbol(supportFz), TermConst(ltl.const(i))),
                    	ltl.expandTemporalFunctionsAtTime(term, ltl.const(i))
                		)	
            		)
        }
        term match {
            case op: UnaryOperator => {
                script = ltl.expandSubformula(op.value, script)  
            }
            case op: BinaryOperator => {
                script = ltl.expandSubformula(op.value1, script)
                script = ltl.expandSubformula(op.value2, script)
            }
            case op: TernaryOperator => {
                script = ltl.expandSubformula(op.value1, script)
                script = ltl.expandSubformula(op.value2, script)
                script = ltl.expandSubformula(op.value3, script)
            }
            case op: MultiOperator => {
                for (sub <- op.values) {
                    script = ltl.expandSubformula(sub, script)  
                }
            }
        }
        return script
    }
}