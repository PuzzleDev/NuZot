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
class LTLLogicDelegate extends LogicDelegate {
    
    /**
     * Adds the following prerequisites at the
     * beginning of the script, assuming that k=5:
     * 
     * (declare-fun iLoop () Int)
     * (declare-fun loopex () Bool)
     * (assert (or (! loopex) (and (< 0 iLoop) (<= iLoop 5))))
     */
    override def generatePreconditions(interpreter: LTLInterpreter): Script = {
	    var script = new Script()
	    
	    // Adds (declare-fun iLoop () Int)
	    script = script :+ CommandDeclareFun(
	            LTLInterpreter.iLoop,
	            List(), interpreter.domain)
	    
	    // Adds (declare-fun loopex () Bool)
	    script = script :+ CommandDeclareFun(
	            LTLInterpreter.loopEx,
	            List(), Sort.Bool)
	    
	    // Adds (assert (or (not loopex) (and (< 0 i_loop) (<= i_loop 5))))
	    //MR: changed to (assert (iff loopex (and (< 0 i_loop) (<= i_loop 5))))
	    script = script :+ CommandAssert(
//	            Or(
//                    Not(Term.call(LTLInterpreter.loopEx)),
              IFF(
                    Term.call(LTLInterpreter.loopEx),
                    And(
                        LT(
                            TermConst(interpreter.const(0)),
                            Term.call(LTLInterpreter.iLoop)
                    	),
                        LE(
                            Term.call(LTLInterpreter.iLoop),
                            TermConst(interpreter.const(interpreter.temporalExt))
                    	)
            		)
                )
	    )
	            
	    return script
	}
    
    //MR: changed: we go bottom-up in the expansion of the subformulae
    //MR: since we do not pre-declare the support functions for the subformulae, we
    //    need to expand the subformulae bottom-up, so that when the support predicates
    //    are introduced in the produced script, they have already been defined
    def expandBooleanTemporalOperator(
            ltl: LTLInterpreter,
            term: BooleanTemporalOperator, computed: Script): Script = {
        
        val supportFz = ltl.generateTemporalSupportFzName(term)
        
        //MR: moved everything after the negated if: if the subformula has already been expanded, there is nothing to do
        if (!ltl.shouldExpandSupportFx(supportFz)) {
          return computed
        }

        var script = computed
        //MR: first we declare the support function and add the LastStateConstraints
        script = ltl.addSupportFzConstraints(supportFz,script)
        
        term match {
            case Since(opX, opY) => {
               //MR: bottom-up
               script = ltl.expandSubformula(opX, script) 
               script = ltl.expandSubformula(opY, script)
               
               script = script :+ CommandAssert(
                    IFF(
                        Term.call(Symbol(supportFz), TermConst(ltl.const(0))),
                    	ltl.expandTemporalFunctionsAtTime(opY,
                    	        ltl.const(0))
                		)	
            		)
//            	if (ltl.shouldExpandSupportFx(supportFz)) {
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
//            	}
//                script = ltl.expandSubformula(opX, script) 
//                script = ltl.expandSubformula(opY, script) 
                return script 
            }
            case Yesterday(op) => {
              //MR: bottom-up
              script = ltl.expandSubformula(op, script)
//                if (ltl.shouldExpandSupportFx(supportFz)) {
	                // 0 < i <= k +1 -> F(i) = x(i - 1)
	                for (i <- 1 until ltl.temporalExt + 2) {
	                    script = script :+ CommandAssert(
	                            IFF(
	                                Term.call(Symbol(supportFz), TermConst(ltl.const(i))),
	                            	ltl.expandTemporalFunctionsAtTime(op, ltl.const(i - 1))
	                        		)	
	                    		)
	                }
	                // (assert (= (f 0) false))
	                script = script :+ CommandAssert(
	                        EQ(
	                            Term.call(Symbol(supportFz), TermConst(ltl.const(0))),
	                        	Term.const(false)
	                    		)	
	                		)
//                }
	            //MR: bottom-up
                return script  
//                return ltl.expandSubformula(op, script)  
            }
            case Zeta(op) => {
              //MR: bottom-up
              script = ltl.expandSubformula(op, script)

                script = script :+ CommandAssert(
                		Term.call(supportFz, TermConst(ltl.const(0)))
            	)
//            	if (ltl.shouldExpandSupportFx(supportFz)) {
	                for (i <- 1 until ltl.temporalExt + 2) {
	                    script = script :+ CommandAssert(
	                            IFF(
	                                Term.call(Symbol(supportFz), TermConst(ltl.const(i))),
	                            	ltl.expandTemporalFunctionsAtTime(op, ltl.const(i - 1))
	                        		)	
	                    		)
	                }
	                // (assert (= (f 0) true))
	                script = script :+ CommandAssert(
	                        EQ(
	                            Term.call(Symbol(supportFz), TermConst(ltl.const(0))),
	                        	Term.const(true)
	                    		)	
	                		)
//            	}
	            //MR: bottom-up
//                return ltl.expandSubformula(op, script) 
                return script 
            }
            case Trigger(opX, opY) => {
                //MR: bottom-up
                script = ltl.expandSubformula(opX, script) 
                script = ltl.expandSubformula(opY, script) 
              
                script = script :+ CommandAssert(
                            IFF(
                                Term.call(Symbol(supportFz), TermConst(ltl.const(0))),
                            	ltl.expandTemporalFunctionsAtTime(opY,
                            	        ltl.const(0))
                        		)	
                    		)
//                if (ltl.shouldExpandSupportFx(supportFz)) {
	                for (i <- 1 until ltl.temporalExt + 2) {
	                    script = script :+ CommandAssert(
	                            IFF(
	                                Term.call(Symbol(supportFz), TermConst(ltl.const(i))),
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
//                }
                //MR: bottom-up
//                script = ltl.expandSubformula(opX, script) 
//                script = ltl.expandSubformula(opY, script) 
                return script
            }
            case Release(opX, opY) => {
              //MR: bottom-up
              script = ltl.expandSubformula(opX, script) 
              script = ltl.expandSubformula(opY, script) 

//                if (ltl.shouldExpandSupportFx(supportFz)) {
	                // 0 <= i <= k -> F(i) = y(i) and (x(1) or F(i + 1))
	                for (i <- 0 until ltl.temporalExt + 1) {
	                    script = script :+ CommandAssert(
	                            IFF(
	                                Term.call(Symbol(supportFz), TermConst(ltl.const(i))),
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
//                }
                
                //(declare-fun i_eve_zot-p0 () Int)
				//(assert (-> loopex (-> (! (zot-p0 5)) (and (<= iLoop i_eve_zot-p0) 
                //		(<= i_eve_zot-p0 5) (! (zot-p0 i_eve_zot-p0)))))
		        val i_eve = "i_eve_" + supportFz
		        script = script :+ CommandDeclareFun(
		                Symbol(i_eve),
		                List(),
		                ltl.domain)
		        script = script ++ ltl.assertDiscreteTemporalValue(Symbol(i_eve))
		                
                //MR: no more needed due to the modification below
//		        val termSymbol = opY match {
//		            case k: TermQualIdentifierTerms => {
//		                k.qualIdentifier.identifier.symbol
//		            }
//		            case _ => {
//		                Symbol(ltl.generateTemporalSupportFzName(opY))
//		            }
//		        }         
		                
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
		                			    		    //MR: avoid using intermediate predicates if possible
		                			    		    ltl.expandTemporalFunctionsAtTime(opY, SpecTermConstant(Term.call(i_eve)))
//		                			    		        Term.call(termSymbol,
//		                			    		        		Term.call(i_eve))
		                			    		)
		                			    )
		                        )
		                )
		        )
		        
		        
		        //MR: bottom-up
//                script = ltl.expandSubformula(opX, script) 
//                script = ltl.expandSubformula(opY, script) 
                return script
            }
            case Next(op) => {
              //MR: bottom-up
              script = ltl.expandSubformula(op, script)
              
//                if (ltl.shouldExpandSupportFx(supportFz)) {
	                // 0 <= i <= k -> F(i) = x(i + 1)
	                for (i <- 0 until ltl.temporalExt + 1) {
	                    script = script :+ CommandAssert(
	                            IFF(
	                                Term.call(Symbol(supportFz), TermConst(ltl.const(i))),
	                            	ltl.expandTemporalFunctionsAtTime(op, ltl.const(i + 1))
	                        		)	
	                    		)
	                }
//                }
	            //MR: bottom-up
//                return ltl.expandSubformula(op, script)  
                return script  
            }
            case Until(opX, opY) => {
              //MR: bottom-up
              script = ltl.expandSubformula(opX, script) 
              script = ltl.expandSubformula(opY, script) 

//                if (ltl.shouldExpandSupportFx(supportFz)) {
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
//                }
                //(declare-fun i_eve_zot-p0 () Int)
				//(assert (-> loopex (-> (zot-p0 5) (and (<= iLoop i_eve_zot-p0)
		        //		(<= i_eve_zot-p0 5) (zot-p0 i_eve_zot-p0)))))
		        val i_eve = "i_eve_" + supportFz
		        script = script :+ CommandDeclareFun(
		                Symbol(i_eve),
		                List(),
		                ltl.domain)
		        script = script ++ ltl.assertDiscreteTemporalValue(Symbol(i_eve))

		        //MR: no more needed due to the modification below
//		        val termSymbol = opY match {
//		            case k: TermQualIdentifierTerms => {
//		                k.qualIdentifier.identifier.symbol
//		            }
//		            case _ => {
//		                Symbol(ltl.generateTemporalSupportFzName(opY))
//		            }
//		        }        
		                
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
		                			    		//MR: avoid using intermediate predicates if possible
		                			    		ltl.expandTemporalFunctionsAtTime(opY, SpecTermConstant(Term.call(i_eve)))
//		                			    		Term.call(termSymbol,
//		                			        		Term.call(i_eve))
		                			    )
		                        )
		                )
		        )
                
		        //MR: bottom-up
//                script = ltl.expandSubformula(opX, script) 
//                script = ltl.expandSubformula(opY, script) 
                return script
            }
        }
    }
    
    def expandBooleanOperator(
            ltl: LTLInterpreter,
            term: BooleanOperator, computed: Script): Script = {
        
        //MR: though Boolean operatoes are not expanded, it is useful to give them a "name" (i.e., a
        //    support predicate, to keep track of the subformulae for which LastStateConstraints have
        //    been already introduced
        val supportFz = ltl.generateTemporalSupportFzName(term)
        var script = computed

        //MR: in case the constraints for the subformula have already been added, we skip all this
        if (!ltl.shouldExpandSupportFx(supportFz)) {
          return computed
        }

        //MR: if we do not introduce a support predicate for the Boolean operator,
        //    we still need to introduce the part of the LastStateConstraint in the case
        //    the loop does not exist
        val lastStateConstraint = CommandAssert(
            IMP(
                Not(Term.call(LTLInterpreter.loopEx)),
                ltl.expandTemporalFunctionsAtTime(
                    Not(term),
                    ltl.const(ltl.temporalExt+1))
                )   
            )

        
        term match {
            case op: And => {
//                if (ltl.shouldExpandSupportFx(supportFz)) {
	                // 0 <= i <= k + 1 -> F(i) = x(i)
                  //MR: we avoid producing the constraints for the predicate representing the
                  //    subformula, as we can use the native Boolean operators of the SMT solver
                  //    (for the time being we are still introducing the predicate letter zot-pX, but we
                  //    do not use it)
                  //    The LoopConstraints are guaranteed because it is enough to have them on the
                  //    arguments of the Boolean operator to have them on the operator itself
                  //    (and x y) is periodic if x and y are periodic
//	                for (i <- 0 until ltl.temporalExt + 2) {
//	                    script = script :+ CommandAssert(
//	                            IFF(
//	                                Term.call(Symbol(supportFz), TermConst(ltl.const(i))),
//	                            	ltl.expandTemporalFunctionsAtTime(op, ltl.const(i))
//	                        		)	
//	                    		)
//	                }
//                }
                for (sub <- op.values) {
                    script = ltl.expandSubformula(sub, script)  
                }
                //MR: we add the LastStateConstraints bottom-up
                return script :+ lastStateConstraint
            }
            case Not(op) => {
//                if (ltl.shouldExpandSupportFx(supportFz)) {
	                // 0 <= i <= k + 1 -> F(i) = x(i)
                  //MR: see comments on And
//	                for (i <- 0 until ltl.temporalExt + 2) {
//	                    script = script :+ CommandAssert(
//	                            IFF(
//	                                Term.call(Symbol(supportFz), TermConst(ltl.const(i))),
//	                            	ltl.expandTemporalFunctionsAtTime(Not(op), ltl.const(i))
//	                        		)	
//	                    		)
//	                }
//                }
                //MR: we add the LastStateConstraints bottom-up
                return ltl.expandSubformula(op, script) :+ lastStateConstraint 
            }
            case op: Rem => {
//                if (ltl.shouldExpandSupportFx(supportFz)) {
                  //MR: see comments on And
//	                for (i <- 0 until ltl.temporalExt + 2) {
//	                    script = script :+ CommandAssert(
//			                EQ(
//						      Term.call(Symbol(supportFz), TermConst(ltl.const(i))),
//						      ltl.expandTemporalFunctionsAtTime(op, ltl.const(i))
//						    )
//	                    )
//	                }
//                }
                script = ltl.expandSubformula(op.left, script)
                script = ltl.expandSubformula(op.right, script)

                //MR: we add the LastStateConstraints bottom-up
                return script :+ lastStateConstraint
            }
            case op: ITE => {
//                if (ltl.shouldExpandSupportFx(supportFz)) {
                  //MR: see comments on And
//	                for (i <- 0 until ltl.temporalExt + 2) {
//	                    script = script :+ CommandAssert(
//			                EQ(
//						      Term.call(Symbol(supportFz), TermConst(ltl.const(i))),
//						      ltl.expandTemporalFunctionsAtTime(op, ltl.const(i))
//						    )
//	                    )
//	                }
//                }
                
                script = ltl.expandSubformula(op.ifa, script)
                script = ltl.expandSubformula(op.thena, script)
                script = ltl.expandSubformula(op.elsea, script)
                //MR: we add the LastStateConstraints bottom-up
                return script :+ lastStateConstraint
            }
            case op: IFF => {
//                if (ltl.shouldExpandSupportFx(supportFz)) {
                  //MR: see comments on And
//	                for (i <- 0 until ltl.temporalExt + 2) {
//	                    script = script :+ CommandAssert(
//			                EQ(
//						      Term.call(Symbol(supportFz), TermConst(ltl.const(i))),
//						      ltl.expandTemporalFunctionsAtTime(op, ltl.const(i))
//						    )
//	                    )
//	                }
//                }
                script = ltl.expandSubformula(op.ifa, script)
                script = ltl.expandSubformula(op.thena, script)
                //MR: we add the LastStateConstraints bottom-up
                return script :+ lastStateConstraint
            }
            case op: IMP => {
//                if (ltl.shouldExpandSupportFx(supportFz)) {
                  //MR: see comments on And
//	                for (i <- 0 until ltl.temporalExt + 2) {
//	                    script = script :+ CommandAssert(
//			                EQ(
//						      Term.call(Symbol(supportFz), TermConst(ltl.const(i))),
//						      ltl.expandTemporalFunctionsAtTime(op, ltl.const(i))
//						    )
//	                    )
//	                }
//                }
                script = ltl.expandSubformula(op.ifa, script)
                script = ltl.expandSubformula(op.thena, script)
                //MR: we add the LastStateConstraints bottom-up
                return script :+ lastStateConstraint
            }
            
            case op: Or => {
//                if (ltl.shouldExpandSupportFx(supportFz)) {
	                // 0 <= i <= k + 1 -> F(i) = x(i)
                  //MR: see comments on And
//	                for (i <- 0 until ltl.temporalExt + 2) {
//	                    script = script :+ CommandAssert(
//	                            IFF(
//	                                Term.call(Symbol(supportFz), TermConst(ltl.const(i))),
//	                                ltl.expandTemporalFunctionsAtTime(op, ltl.const(i))
//	                        		)	
//	                    		)
//	                }
//                }
                for (sub <- op.values) {
                    script = ltl.expandSubformula(sub, script)  
                }
                //MR: we add the LastStateConstraints bottom-up
                return script :+ lastStateConstraint
            }
            case op: Xor => {
                throw new IllegalStateException("Not implemented")
            }
        }
    }
}