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
class LTLLogicDelegate extends LogicDelegate {

    
    /**
     * Adds the following prerequisites at the
     * beginning of the script, assuming that k=5:
     * 
     * (declare-fun iLoop () Int)
     * (declare-fun loopex () Bool)
     * (assert (or (! loopex) (and (< 0 iLoop) (<= iLoop 5))))
     */
    override def generatePreconditions(ltl: LTLInterpreter): Script = {
	    var script = new Script()
	    
	    // Adds (declare-fun iLoop () Int)
	    script = script :+ CommandDeclareFun(
	            LTLInterpreter.iLoop,
	            List(), ltl.domain)
	    
	    // Adds (declare-fun loopex () Bool)
	    script = script :+ CommandDeclareFun(
	            LTLInterpreter.loopEx,
	            List(), Sort.Bool)
	    
	    // Adds (assert (or (not loopex) (and (< 0 i_loop) (<= i_loop 5))))
	    script = script :+ CommandAssert(
	            Or(
                    Not(Term.call(LTLInterpreter.loopEx)),
                    And(
                        LT(
                            TermConst(ltl.const(0)),
                            Term.call(LTLInterpreter.iLoop)
                    	),
                        LE(
                            Term.call(LTLInterpreter.iLoop),
                            TermConst(ltl.const(ltl.temporalExt))
                    	)
            		)
                )
	    )
	            
	    return script
	}
    
    def expandTemporalBooleanOperator(
            ltl: LTLInterpreter,
            term: TemporalOperator, computed: Script): Script = {
        
        val supportFz = ltl.generateTemporalSupportFzName(term)
        var script = computed
        
        term match {
            case Since(opX, opY) => {
               script = script :+ CommandAssert(
                    IFF(
                        Term.call(Symbol(supportFz), TermConst(ltl.const(0))),
                    	ltl.expandTemporalFunctionsAtTime(opY,
                    	        ltl.const(0))
                		)	
            		)
                for (i <- 1 until ltl.temporalExt + 2) {
                    script = script :+ CommandAssert(
                            IFF(
                                Term.call(Symbol(supportFz), TermConst(ltl.const(i))),
                            	ltl.expandTemporalFunctionsAtTime(
                            			Or(opY, 
                            			    And(opX, 
                            			        Term.call(Symbol(supportFz),
                            			                TermConst(ltl.const(i - 1)))
                            			    )
                            			),
                            	        ltl.const(i))
                        		)	
                    		)
                }
                script = ltl.expandSubformula(opX, script) 
                script = ltl.expandSubformula(opY, script) 
                return script 
            }
            case Yesterday(op) => {
                // 0 < i <= k +1 -> F(i) = x(i - 1)
                for (i <- 1 until ltl.temporalExt + 2) {
                    script = script :+ CommandAssert(
                            IFF(
                                Term.call(Symbol(supportFz), TermConst(ltl.const(i))),
                            	ltl.expandTemporalFunctionsAtTime(op, ltl.const(i - 1))
                        		)	
                    		)
                }
                return ltl.expandSubformula(op, script)  
            }
            case Zeta(op) => {
                script = script :+ CommandAssert(
                		Term.call(supportFz, TermConst(ltl.const(0)))
            	)
                for (i <- 1 until ltl.temporalExt + 2) {
                    script = script :+ CommandAssert(
                            IFF(
                                Term.call(Symbol(supportFz), TermConst(ltl.const(i))),
                            	ltl.expandTemporalFunctionsAtTime(op, ltl.const(i - 1))
                        		)	
                    		)
                }
                return ltl.expandSubformula(op, script) 
            }
            case Trigger(opX, opY) => {		                
                script = script :+ CommandAssert(
                            IFF(
                                Term.call(Symbol(supportFz), TermConst(ltl.const(0))),
                            	ltl.expandTemporalFunctionsAtTime(opY,
                            	        ltl.const(0))
                        		)	
                    		)
                for (i <- 1 until ltl.temporalExt + 2) {
                    script = script :+ CommandAssert(
                            IFF(
                                Term.call(Symbol(supportFz), Term.const(i)),
                            	ltl.expandTemporalFunctionsAtTime(
                            			And(opY, 
                            			    Or(opX, 
                            			        Term.call(Symbol(supportFz),
                            			                TermConst(ltl.const(i - 1)))
                            			    )
                            			),
                            	        ltl.const(i))
                        		)	
                    		)
                }
                script = ltl.expandSubformula(opX, script) 
                script = ltl.expandSubformula(opY, script) 
                return script
            }
            case Release(opX, opY) => {
                // 0 <= i <= k -> F(i) = y(i) and (x(1) or F(i + 1))
                for (i <- 0 until ltl.temporalExt + 1) {
                    script = script :+ CommandAssert(
                            IFF(
                                Term.call(Symbol(supportFz), Term.const(i)),
                            	ltl.expandTemporalFunctionsAtTime(
                            			And(opY, 
                            			    Or(opX, 
                            			        Term.call(Symbol(supportFz),
                            			                TermConst(ltl.const(i + 1)))
                            			    )
                            			),
                            	        ltl.const(i))
                        		)	
                    		)
                }
                
                //(declare-fun i_eve_zot-p0 () Int)
				//(assert (-> loopex (-> (! (zot-p0 5)) (and (<= iLoop i_eve_zot-p0) 
                //		(<= i_eve_zot-p0 5) (! (zot-p0 i_eve_zot-p0)))))
		        val i_eve = "i_eve_" + supportFz
		        script = script :+ CommandDeclareFun(
		                Symbol(i_eve),
		                List(),
		                ltl.domain)
		        script = script :+ CommandAssert(
		                IMP(
		                        Term.call(LTLInterpreter.loopEx),
		                        IMP(
		                                Not(
			                                Term.call(Symbol(supportFz),
			                			        		TermConst(ltl.const(ltl.temporalExt)))
			                			),
		                			    And(
		                			    		LE(
		                			    		        Term.call(LTLInterpreter.iLoop),
		                			    		        Term.call(i_eve)
		                			    		),
		                			    		LE(
		                			    				Term.call(i_eve),
		                			    				TermConst(ltl.const(ltl.temporalExt))
		                			    		),
		                			    		Not(
		                			    		        Term.call(Symbol(supportFz),
		                			    		        		Term.call(i_eve))
		                			    		)
		                			    )
		                        )
		                )
		        )
                
                script = ltl.expandSubformula(opX, script) 
                script = ltl.expandSubformula(opY, script) 
                return script
            }
            case Next(op) => {		                
                // 0 <= i <= k -> F(i) = x(i + 1)
                for (i <- 0 until ltl.temporalExt + 1) {
                    script = script :+ CommandAssert(
                            IFF(
                                Term.call(Symbol(supportFz), TermConst(ltl.const(i))),
                            	ltl.expandTemporalFunctionsAtTime(op, ltl.const(i + 1))
                        		)	
                    		)
                }
                return ltl.expandSubformula(op, script)  
            }
            case Until(opX, opY) => {
                // 0 <= i <= k -> F(i) = y(i) or (x(1) and F(i + 1))
                for (i <- 0 until ltl.temporalExt + 1) {
                    script = script :+ CommandAssert(
                        IFF(
                            Term.call(Symbol(supportFz), TermConst(ltl.const(i))),
                        	ltl.expandTemporalFunctionsAtTime(
                        			Or(opY, 
                        			    And(opX, 
                        			        Term.call(Symbol(supportFz),
                        			        		TermConst(ltl.const(i + 1)))
                        			    )
                        			),
                        	        ltl.const(i))
                    		)	
                		)
                }
                //(declare-fun i_eve_zot-p0 () Int)
				//(assert (-> loopex (-> (zot-p0 5) (and (<= iLoop i_eve_zot-p0)
		        //		(<= i_eve_zot-p0 5) (zot-p0 i_eve_zot-p0)))))
		        val i_eve = "i_eve_" + supportFz
		        script = script :+ CommandDeclareFun(
		                Symbol(i_eve),
		                List(),
		                ltl.domain)
		        script = script :+ CommandAssert(
		                IMP(
		                        Term.call(LTLInterpreter.loopEx),
		                        IMP(
		                                Term.call(Symbol(supportFz),
		                			        		TermConst(ltl.const(ltl.temporalExt))),
		                			    And(
		                			    		LE(
		                			    		        Term.call(LTLInterpreter.iLoop),
		                			    		        Term.call(i_eve)
		                			    		),
		                			    		LE(
		                			    				Term.call(i_eve),
		                			    				TermConst(ltl.const(ltl.temporalExt))
		                			    		),
		                			    		Term.call(Symbol(supportFz),
		                			        		Term.call(i_eve))
		                			    )
		                        )
		                )
		        )
                
                script = ltl.expandSubformula(opX, script) 
                script = ltl.expandSubformula(opY, script) 
                return script
            }
        }
    }
    
    def expandBooleanOperator(
            ltl: LTLInterpreter,
            term: BooleanOperator, computed: Script): Script = {
        
        val supportFz = ltl.generateTemporalSupportFzName(term)
        var script = computed
                
        term match {
            case op: And => {
                // 0 <= i <= k + 1 -> F(i) = x(i)
                for (i <- 0 until ltl.temporalExt + 2) {
                    script = script :+ CommandAssert(
                            IFF(
                                Term.call(Symbol(supportFz), TermConst(ltl.const(i))),
                            	ltl.expandTemporalFunctionsAtTime(op, ltl.const(i))
                        		)	
                    		)
                }
                for (sub <- op.values) {
                    script = ltl.expandSubformula(sub, script)  
                }
                return script 
            }
            case Not(op) => {
                // 0 <= i <= k + 1 -> F(i) = x(i)
                for (i <- 0 until ltl.temporalExt + 2) {
                    script = script :+ CommandAssert(
                            IFF(
                                Term.call(Symbol(supportFz), TermConst(ltl.const(i))),
                            	ltl.expandTemporalFunctionsAtTime(Not(op), ltl.const(i))
                        		)	
                    		)
                }
                return ltl.expandSubformula(op, script)  
            }
            case op: Rem => {
                throw new IllegalStateException("Not implemented")
            }
            case op: ITE => {
                throw new IllegalStateException("Not implemented")
            }
            case op: IFF => {
                throw new IllegalStateException("Not implemented")
            }
            case op: IMP => {
                throw new IllegalStateException("Not implemented")
            }
            
            case op: Or => {
                // 0 <= i <= k + 1 -> F(i) = x(i)
                for (i <- 0 until ltl.temporalExt + 2) {
                    script = script :+ CommandAssert(
                            IFF(
                                Term.call(Symbol(supportFz), TermConst(ltl.const(i))),
                            	ltl.expandTemporalFunctionsAtTime(op, ltl.const(i))
                        		)	
                    		)
                }
                for (sub <- op.values) {
                    script = ltl.expandSubformula(sub, script)  
                }
                return script
            }
            case op: Xor => {
                throw new IllegalStateException("Not implemented")
            }
        }
    }
}