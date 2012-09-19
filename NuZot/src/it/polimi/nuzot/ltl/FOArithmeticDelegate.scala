/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.ltl

import it.polimi.nuzot.ltl.grammar._
import it.polimi.nuzot.smt.grammar._
import it.polimi.nuzot.core.DSLInterpreter

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 */
class FOArithmeticDelegate extends ArithmeticDelegate {

    override def generatePreconditions(interpreter: LTLInterpreter): Script = {
	    return new Script()
    }
    
    override def expandArithmeticTemporalOperator(
            ltl: LTLInterpreter,
            term: ArithmeticTemporalOperator,
            computed: Script): Script = {

        // Adds (declare-fun zot-pX (Int) <term_domain>)
        def declareSupportFz(supportFz: String, returnType: Sort): Command = {          
            return CommandDeclareFun(
                    Symbol(supportFz),
                    List(ltl.domain),
                    returnType)
        }

        //MR: retrieves the type of the term, by looking at the function
        def getTermType(term: Term) : Sort = {
          term match {
            case NextV(tm) =>
              { return getTermType(tm) }
            case YesterdayV(tm) =>
              { return getTermType(tm) }
            case tm: TermQualIdentifierTerms => {
              val fzName = tm.qualIdentifier.identifier.symbol
              val returnType = ltl.temporalFunctions.get(fzName)

              returnType match {
                case Some(k) => k
                case _ =>
                  throw new IllegalArgumentException(term + " not found")
              }
            }  
            case _ =>
              throw new IllegalArgumentException(term + " syntax error: arithmetic operator applied to wrong term")

          }
        }
        val supportFz = ltl.generateTemporalSupportFzName(term)

        //MR: moved everything after the negated if: if the subformula has already been expanded, there is nothing to do
        if (!ltl.shouldExpandSupportFx(supportFz)) {
          return computed
        }

        //MR: for the time being I am using ltl.domain as range for the term, but this
        //    should be changed
        var script = computed :+ declareSupportFz(supportFz, getTermType(term))

        term match {
            case NextV(op) => {
              //MR: since we do not pre-declare the support functions for the subformulae, we
              //    need to expand the subformulae bottom-up, so that when the support predicates
              //    are introduced in the produced script, they have already been defined
              script = ltl.expandSubformula(op, script)
              
              // 0 <= i <= k -> F(i) = x(i + 1)
              for (i <- 0 until ltl.temporalExt + 1) {
                script = script :+ CommandAssert(
                    EQ(
                        Term.call(Symbol(supportFz), TermConst(ltl.const(i))),
                        ltl.expandTemporalFunctionsAtTime(op, ltl.const(i + 1))
                        )   
                    )
              }

              return script  
            }
            case YesterdayV(op) => {
              //MR: bottom-up
              script = ltl.expandSubformula(op, script)

                  // 0 < i <= k +1 -> F(i) = x(i - 1)
                  for (i <- 1 until ltl.temporalExt + 2) {
                    script = script :+ CommandAssert(
                        EQ(
                            Term.call(Symbol(supportFz), TermConst(ltl.const(i))),
                            ltl.expandTemporalFunctionsAtTime(op, ltl.const(i - 1))
                            )   
                        )
                  }

                return script  
            }
            case _ => {
              throw new IllegalArgumentException(term + 
                  " not implemented yet")
            }
        }

//        throw new IllegalArgumentException(term + 
//                        " not implemented yet")
    }
            
    override def expandArithmeticOperator(
            ltl: LTLInterpreter,
            term: ArithmeticOperator,
            computed: Script): Script = {
        val supportFz = ltl.generateTemporalSupportFzName(term)

        //MR: moved everything after the negated if: if the subformula has already been expanded, there is nothing to do
        if (!ltl.shouldExpandSupportFx(supportFz)) {
          return computed
        }
        
        var script = computed
        //MR: first we declare the support function and add the LastStateConstraints
        script = ltl.addSupportFzConstraints(supportFz,script)

        //MR: moved at the bottom
//        if (ltl.shouldExpandSupportFx(supportFz)) {
//	        // 0 <= i <= k -> zot-pX(i) <-> (AOP x(i) y(i))
//	        for (i <- 0 until ltl.temporalExt + 2) {
//	            script = script :+ CommandAssert(
//	                    IFF(
//	                        Term.call(Symbol(supportFz), TermConst(ltl.const(i))),
//	                    	ltl.expandTemporalFunctionsAtTime(term, ltl.const(i))
//	                		)	
//	            		)
//	        }
//        }
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
        //MR: since we do not pre-declare the support functions for the subformulae, we
        //    need to expand the subformulae bottom-up, so that when the support predicates
        //    are introduced in the produced script, they have already been defined
        // 0 <= i <= k -> zot-pX(i) <-> (AOP x(i) y(i))
        for (i <- 0 until ltl.temporalExt + 2) {
          script = script :+ CommandAssert(
              IFF(
                  Term.call(Symbol(supportFz), TermConst(ltl.const(i))),
                  ltl.expandTemporalFunctionsAtTime(term, ltl.const(i))
                  )   
              )
              }

        return script
    }
}