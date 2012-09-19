/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.ltl

import it.polimi.nuzot.smt.grammar._
import it.polimi.nuzot.core.DSLInterpreter

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 */
class DefaultEqualityDelegate extends EqualityDelegate {

    override def generatePreconditions(interpreter: LTLInterpreter): Script = {
	    return new Script()
    }
    
    override def expandEqualityOperator(
            ltl: LTLInterpreter,
            term: EqualityOperator, computed: Script): Script = {
        val supportFz = ltl.generateTemporalSupportFzName(term)

        //MR: moved everything after the negated if: if the subformula has already been expanded, there is nothing to do
        if (!ltl.shouldExpandSupportFx(supportFz)) {
          return computed
        }
        
        var script = computed
        //MR: as for the BooleanOperators, we avoid introducing the support predicate, as the
        //    SMT solver can natively handle the (in)equality predicates
//        // Same as aritmetic
//        // 0 <= i <= k -> zot-pX(i) <-> (AOP x(i) y(i))
//        for (i <- 0 until ltl.temporalExt + 2) {
//            script = script :+ CommandAssert(
//                    IFF(
//                        Term.call(Symbol(supportFz), TermConst(ltl.const(i))),
//                    	ltl.expandTemporalFunctionsAtTime(term, ltl.const(i))
//                		)	
//            		)
//        }
        
        //MR: if we do not introduce a support predicate for the Boolean operator,
        //    we still need to introduce the LastStateConstraints (both in the case in which
        //    the loop exists and the on in which the loop does not exist, as the values of the
        //    terms might not be periodic
        val lastStateConstraintNotLoopEx = CommandAssert(
            IMP(
                Not(Term.call(LTLInterpreter.loopEx)),
                ltl.expandTemporalFunctionsAtTime(
                    Not(term),
                    ltl.const(ltl.temporalExt+1))
                )   
            )
        val lastStateConstraintLoopEx = CommandAssert(
            IMP(
                Term.call(LTLInterpreter.loopEx),
                IFF(
                    ltl.expandTemporalFunctionsAtTime(
                        term,
                        SpecTermConstant(Term.call(LTLInterpreter.iLoop))),
                    ltl.expandTemporalFunctionsAtTime(
                        term,
                        ltl.const(ltl.temporalExt+1))
                )))
        //MR: we also need to add the loopConstraint on the (in)equality
        val loopConstraint = CommandAssert(
            IMP(
                Term.call(LTLInterpreter.loopEx),
                IFF(
                    ltl.expandTemporalFunctionsAtTime(
                        term,
                        SpecTermConstant(Sub(Term.call(LTLInterpreter.iLoop), TermConst(ltl.const(1))))),
                    ltl.expandTemporalFunctionsAtTime(
                        term,
                        ltl.const(ltl.temporalExt))
                ))
            )

        script = ltl.expandSubformula(term.value1, script)
        script = ltl.expandSubformula(term.value2, script)

        //MR: we add the LastStateConstraints bottom-up
        return script :+ lastStateConstraintNotLoopEx :+ lastStateConstraintLoopEx :+ loopConstraint
    }
}