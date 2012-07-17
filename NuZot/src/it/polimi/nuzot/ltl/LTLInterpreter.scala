/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.ltl

import java.lang.reflect.Constructor

import it.polimi.nuzot.core._
import it.polimi.nuzot.ltl.grammar._
import it.polimi.nuzot.smt.grammar._


/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 * <p>Convert LTL operators into SMT-lib.
 * 
 * <p>The conversion process works as follows:
 * <li>A temporal extension coefficient K is defined. K is a constant
 * over which the LTL instructions are expanded.
 * <li>For each past temporal operator including not atomic 
 * temporal functions at time 0 <code>LTLInterpreter.genPastAt0</code>
 * is recursively invoked. For times 1..K <code>LTLInterpreter.genPast</code>
 * is recursively invoked.
 * <li>For each future operator for times 0..K 
 * <code>LTLInterpreter.genFuture</code> is recursively invoked.
 * <li>If an i-loop time is defined for each atomic
 * function is added p(i_loop - 1) <-> p(K)
 */
object LTLInterpreter {
    val kKeyword = Keyword(":k")
    val domainKeyword = Keyword(":domain")
        
    val iLoopLabel = "iLoop"
    val iLoop = Symbol(iLoopLabel)
        
    val loopExLabel = "loopex"
    val loopEx = Symbol(loopExLabel)
    
    val subformulaPrefix = "zot-p"
}

class LTLInterpreter() extends DSLInterpreter {
    
    // Defines the numeric domain
    // The default one is Integer
    private var _domain: Sort = Sort.Int
    
    // A boolean logic delegate
    private var _logicDelegate: LogicDelegate = 
        	new LTLLogicDelegate()
    
    // Arithmetic delegate
    private var _arithmeticDelegate: ArithmeticDelegate = 
        	new FOArithmeticDelegate()
    
    // Equality delegate
    private var _equalityDelegate: EqualityDelegate =
        	new DefaultEqualityDelegate()
    
    // Declared temporal functions.
    var temporalFunctions = new Scope[Symbol, Sort]()    
    
    // The temporal extension K
    var _temporalExt: Int = -1
    
    def domain(): Sort = {
    	_domain
    }
    
    def domain(sort: Sort): Unit = {
    	if (!isInitializing()) {
    	    throw new IllegalStateException(
    	            "Trying to change the domain while not initializing.")
    	}
    }
    
    def logicDelegate(): LogicDelegate = {
    	return _logicDelegate
    }
    
    def arithmeticDelegate(): ArithmeticDelegate = {
        return _arithmeticDelegate
    }
    
    def equalityDelegate(): EqualityDelegate = {
        return _equalityDelegate
    }
    
    def temporalExt(): Int = {
        if (_temporalExt < 0) {
            throw new IllegalStateException(
                  LTLInterpreter.kKeyword + " not set. " +
                  "Add (set-info " + LTLInterpreter.kKeyword + 
                  ") before any command.")
        } else {
            _temporalExt
        }
    }
    
    def temporalExt(value : Int): Unit = {
        if (_temporalExt != -1 ) {
            throw new IllegalStateException(
                  LTLInterpreter.kKeyword + " was already set.")
        } else {
        	_temporalExt = value
        }
    }
    
    def const(value: Int): SpecConstant = {
        domain match {
            case Sort.Int => {
                return SpecIntConstant(value)
            }
            case Sort.Real => {
                return SpecDoubleConstant(value)                
            }
        }
    }
    
    protected def initializeAttributes(): Unit = {
        // Set the number domain
        attributes.get(LTLInterpreter.domainKeyword) match {
            case Some(AttributeKeyVal(k, AttributeValueSymbol(Symbol.Int))) => {
                _domain = Sort.Int
            }
            case Some(AttributeKeyVal(k, AttributeValueSymbol(Symbol.Real))) => {
                _domain = Sort.Real
            }
            case Some(x) => {
                throw new IllegalArgumentException(
                        "Invalid " + LTLInterpreter.domainKeyword + ": " + x + ".")
            }
            case None => {
                // Using default
            }
        }
        
        // Set the temporal extension
        attributes.get(LTLInterpreter.kKeyword) match {
            case Some(AttributeKeyVal(k, AttributeValueSpecConst(const))) => {
                domain match {
                    case Sort.Int => {
                        const match {
                            case SpecIntConstant(value) => {
                                temporalExt(value)
                            }
                            case _ => {
                                throw new IllegalArgumentException(
                                		LTLInterpreter.kKeyword + " wrong type: " + 
                                		const + ".")
                            }
                        }
                    }
                    case Sort.Real => {
                        const match {
                            case SpecDoubleConstant(value) => {
                                temporalExt(value.toInt)
                            }
                            case _ => {
                                throw new IllegalArgumentException(
                                		LTLInterpreter.kKeyword + " wrong type: " + 
                                		const + ".")
                            }
                        }
                    }
                }
                
            }
            case _ => {
                // Nothing to be done
            }
        }
    }
    
    protected def initializeDelegates(): Unit = {
        // Load all the delegates
        for (delegate <- delegateClasses) {
            delegate match {
        	    case x: ArithmeticDelegate => {
        	        _arithmeticDelegate = x
        	    }
        	    case x: LogicDelegate => {
        	        _logicDelegate = x
        	    }
        	    case x: EqualityDelegate => {
        	        _equalityDelegate = x
        	    }
        	    case _ => {
        	        // Pass
        	        /*
        	        throw new IllegalArgumentException(
        	                "Unknown delegate type: " + className + ".")
        	               */
        	    }
        	}	
        }
    }
    
    
    
    def loadLogic(logic: Symbol) = {
        val className = logic.toString()
        try {
        	val clazz = Class.forName(className)
        	val instance = clazz.newInstance()
        	instance match {
        	    case x: ArithmeticDelegate => {
        	        _arithmeticDelegate = x
        	    }
        	    case x: LogicDelegate => {
        	        _logicDelegate = x
        	    }
        	    case x: EqualityDelegate => {
        	        _equalityDelegate = x
        	    }
        	    case _ => {
        	        // Pass
        	        /*
        	        throw new IllegalArgumentException(
        	                "Unknown delegate type: " + className + ".")
        	               */
        	    }
        	}	
        } catch {
            case ex: IllegalArgumentException => {
                throw ex;
            }
            // TODO(michele.sama): handle various exception differently
		    case ex: Exception => {
		        throw new IllegalArgumentException(
		                "Delegate " + className + 
		                " not found or could not be instantiated.",
		                ex)
		    }
		}
    }
    
    /**
     * Adds the following prerequisites at the
     * beginning of the script, assuming that k=5:
     * 
     * (declare-fun iLoop () Int)
     * (declare-fun loopex () Bool)
     * (assert (or (! loopex) (and (< 0 iLoop) (<= iLoop 5))))
     */
    override def generatePreconditions(): Script = {
	    var script = new Script()
	    
	    return script ++ equalityDelegate.generatePreconditions(this) ++
	    		arithmeticDelegate.generatePreconditions(this) ++
	    		logicDelegate.generatePreconditions(this) ++
	    		assertDiscreteTemporalValue(LTLInterpreter.iLoop)	    
	}
    
    /**
     * Force a numeric symbol to be assigned only with a
     * discrete value from 0 to K+1.
     */
    def assertDiscreteTemporalValue(symbol: Symbol): Script = {
        domain match {
	        case Sort.Real => {
	            var terms: List[Term] = List()
	            for (i <- 0 to _temporalExt + 1) {
	               terms = EQ(Term.call(symbol),
	                         TermConst(const(i))) :: terms
	            }
	            return Script(List(
	                    CommandAssert(Or(terms: _*))))
	        } case _ => {
	            return new Script()
	        }
	    }
    }
    
    /**
     * Recursively increase the arity of all the temporal operators and 
     * sets the given time.
     */
    def expandTemporalFunctionsAtTime(
            sourceFz: Term, time: SpecConstant): Term = {        
        sourceFz match {
            // Constant
            case x: TermConst => {
                return x
            }
            case x: TermQualIdentifierTerms => {
                // Expand (x k) in (x time k)
                val symbol = temporalFunctions.get(
                        x.qualIdentifier.identifier.symbol)
                symbol match {
                    case Some(_) => {
                        return TermQualIdentifierTerms(
                                x.qualIdentifier,
                                (Seq(TermConst(time)) ++ x.terms): _*) // Ugly
                    }
                    case None => {
                        // Not a temporal function
                        return x
                    }
                }
            }
            case x: BooleanTemporalOperator => {
               val supportFz = generateTemporalSupportFzName(x)
               return TermQualIdentifierTerms(
                        QualIdentifierIdentifier(IdentifierSymbol(Symbol(supportFz))),
                        TermConst(time))
            }
            case x: ArithmeticTemporalOperator => {
               val supportFz = generateTemporalSupportFzName(x)
               return TermQualIdentifierTerms(
                        QualIdentifierIdentifier(IdentifierSymbol(Symbol(supportFz))),
                        TermConst(time))
            }
            // explore all the sub 
            case x: UnaryOperator => {
				val clazz = x.getClass() 
				val constructor = clazz.getConstructor(classOf[Term])
				return constructor.newInstance(
				        expandTemporalFunctionsAtTime(x.value, time))
            }
            case x: BinaryOperator => {
                val clazz = x.getClass() 
				val constructor = clazz.getConstructor(
				        classOf[Term], classOf[Term])
				return constructor.newInstance(
				        expandTemporalFunctionsAtTime(x.value1, time),
				        expandTemporalFunctionsAtTime(x.value2, time))
            }
            case x: TernaryOperator => {
                val clazz = x.getClass() 
				val constructor = clazz.getConstructor(
				        classOf[Term], classOf[Term], classOf[Term])
				return constructor.newInstance(
				        expandTemporalFunctionsAtTime(x.value1, time),
				        expandTemporalFunctionsAtTime(x.value2, time), 
				        expandTemporalFunctionsAtTime(x.value3, time))
            }
            // TODO(m.sama): remove this
            // Read below for further explanation.
            case x: Add => {
                return Add({for (k <- x.values) yield 
                		expandTemporalFunctionsAtTime(k, time)}: _*)
            }
            case x: And => {
                return And({for (k <- x.values) yield 
				        expandTemporalFunctionsAtTime(k, time)}: _*)
            }
            case x: Mul => {
                return Mul({for (k <- x.values) yield 
				        expandTemporalFunctionsAtTime(k, time)}: _*)
            }
            case x: Or => {
                return Or({for (k <- x.values) yield 
				        expandTemporalFunctionsAtTime(k, time)}: _*)
            }
            case x: Sub => {
                return Sub({for (k <- x.values) yield 
				        expandTemporalFunctionsAtTime(k, time)}: _*)
            }
            // TODO(m.sama): the following gives an error.
            // The error is related to the implicit conversion between
            // scala's and java's varargs.
            // When this will work we can remove all the above MultiOperator
            case x: MultiOperator => {
                val clazz = x.getClass() 
				val constructor = clazz.getConstructor(classOf[Seq[Term]])
				return constructor.newInstance(
				        {for (k <- x.values) yield 
				            expandTemporalFunctionsAtTime(k, time)}: _*)
            }
            case _ => {
                throw new IllegalArgumentException(sourceFz + 
                        " did we forget about something?")
            }
        }
    }
    
    //Internal counter used to generate unique support function names
    var supportFzCounter = 0
    var supportFzScope = new Scope[Term, String]
    def generateTemporalSupportFzName(term: Term): String = {
        supportFzScope.get(term) match {
            case Some(name) => {
                return name
            }
            case None => {}
        }
        
        // Not thread safe
        val fzName = LTLInterpreter.subformulaPrefix + supportFzCounter
        supportFzCounter += 1
        
        supportFzScope.set(term, fzName)
        return fzName
    }
    
    def generateAllTemporalSupportFz(
            sourceFz: Term, computed: Script, 
            satisfy: Boolean, satisfyAt: SpecConstant): Script = {
        
        // clojure
        // Adds satisfiability at state
        def assertSatisfyAt(supportFz: String, computed: Script): Script = {
            if (satisfy) {
                return computed :+ CommandAssert(
                        Term.call(supportFz, TermConst(satisfyAt))
                    	)
            } else {
                return computed
            }
        }
        
        // Adds (declare-fun zot-pX (Int) Bool)
        def declareSupportFz(supportFz: String, computed: Script): Script = {
            return computed :+ CommandDeclareFun(
                    Symbol(supportFz),
                    List(domain),
                    Sort.Bool)
        }
         
        sourceFz match {
	        case x: TermConst => {
	                return computed
	            }
            // Atomic and parametric functions
            case x: TermQualIdentifierTerms => {
                val fzName = x.qualIdentifier.identifier.symbol
                val returnType = temporalFunctions.get(fzName)
                returnType match {
                    case Some(s) => {
                        var script = computed
                        if (satisfy) {
                            script = assertSatisfyAt(fzName.name, script)
                        }
                        return script
                    }
                    case None => {
                        return computed
                    }
                }
            }
            case x: KnownFunctionInvocation => {
                val supportFz = generateTemporalSupportFzName(x)
                var script = computed
                script = declareSupportFz(supportFz, script)
                script = assertSatisfyAt(supportFz, script)
                
                // (assert (-> loopex (iff (zot-f0 6) (zot-f0 iLoop)))
                script = script :+ CommandAssert(
                        IMP(
                        	Term.call(LTLInterpreter.loopEx),
	                        IFF(
	                        	Term.call(Symbol(supportFz), TermConst(const(temporalExt + 1))),
	                        	Term.call(Symbol(supportFz), Term.call(LTLInterpreter.iLoop))
	                        )
                        )
                	)
     			// (assert (-> (! loopex) (! (zot-f0 6)))
                script = script :+ CommandAssert(
                        IMP(
                        	Not(Term.call(LTLInterpreter.loopEx)),
                        	Not(Term.call(Symbol(supportFz), TermConst(const(temporalExt + 1))))
                        )
                	)
                
                x match {
                    case op: UnaryOperator => {
		                return generateAllTemporalSupportFz(
		                        op.value, script, false, satisfyAt)  
		            }
                    case op: BinaryOperator => {
		                script = generateAllTemporalSupportFz(
		                        op.value1, script, false, satisfyAt) 
		                script = generateAllTemporalSupportFz(
		                        op.value2, script, false, satisfyAt) 
		                return script 
                    }
                    case op: TernaryOperator => {
		                script = generateAllTemporalSupportFz(
		                        op.value1, script, false, satisfyAt) 
		                script = generateAllTemporalSupportFz(
		                        op.value2, script, false, satisfyAt) 
		                script = generateAllTemporalSupportFz(
		                        op.value3, script, false, satisfyAt) 
		                return script 
                    }
                    case op: MultiOperator => {
                        for (value <- op.values) {
                            script = generateAllTemporalSupportFz(value, script, false, satisfyAt) 
                        }
		                return script 
                    }
                    case _ => {
                        throw new IllegalArgumentException(
                                "Did we forget about something: " + x)
                    }
		            
                }
            }
            case _ => {
                throw new IllegalArgumentException(sourceFz + 
                        " did we forget about something?")
            }
        }
    }
    
    /**
     * Expand the subformula at a given time.
     * Parametric and recursive. 
     */
    def expandSubformula(sourceFz: Term, computed: Script): Script = {  
        sourceFz match {
            // Constant
            case x: TermConst => {
                return computed
            }
            // Atomic and parametric functions
            case x: TermQualIdentifierTerms => {
                // TODO(m.sama): avoid multiple atoms constraints
                val fzName = x.qualIdentifier.identifier.symbol
                val returnType = temporalFunctions.get(fzName)
                returnType match {
                    case Some(s) => {
                        s match {
                            case Sort.Bool => {
                                return computed :+ CommandAssert(
	                                And(
	                            		Term.call(LTLInterpreter.loopEx),
	                    				IFF(
	                						Term.call(
	            						        fzName,
	            						        (Seq(Sub(
	            						                Term.call(LTLInterpreter.iLoop),
	            						                TermConst(const(1))
	            						        ))++x.terms): _*),
	                						Term.call(
	            						        fzName,
	            						        (Seq(TermConst(const(temporalExt))) ++ x.terms): _*
	            						        )
	                    				)
	                                )
	                            )
                            }
                            case Sort.Int | Sort.Real => {
                                return computed
                                // TODO(m.sama): add a flag
                                // The arithmetic part is not periodic unless manually requested.
                                /*
                                return computed :+ CommandAssert(
	                                And(
	                            		Term.call(LTLInterpreter.loopEx),
	                    				EQ(
	                						Term.call(
	            						        fzName,
	            						        (Seq(Sub(
	            						                Term.call(LTLInterpreter.iLoop),
	            						                Term.const(1)
	            						        ))++x.terms): _*),
	                						Term.call(
	            						        fzName,
	            						        (Seq(Term.const(temporalExt))++x.terms): _*
	            						        )
	                    				)
	                                )
	                            )*/
                            }
                            case _ => {
                                throw new IllegalArgumentException(
                                        "Invalid sort: " + s)
                            }
                        }
                        
                        
                    }
                    case None => {
                        return computed
                    }
                }
            }
            case x: BooleanTemporalOperator => {
                return logicDelegate.expandBooleanTemporalOperator(this, x, computed)
            }
            // Boolean operators
            case x: BooleanOperator => {
                return logicDelegate.expandBooleanOperator(this, x, computed)
            }
            
            // Aritmetic operator
            case x: ArithmeticTemporalOperator => {
                 return arithmeticDelegate.expandArithmeticTemporalOperator(
                         this, x, computed)
            }
            case x: ArithmeticOperator => {
                return arithmeticDelegate.expandArithmeticOperator(
                         this, x, computed)
            }
            case x: EqualityOperator => {
                return equalityDelegate.expandEqualityOperator(
                         this, x, computed)
            }
            case _ => {
                throw new IllegalArgumentException(sourceFz + 
                        " did we forget about something?")
            }
        }
    }
    
    def doExpandLTL(sourceFz: Term, computed: Script): Script = {
        val script = generateAllTemporalSupportFz(
                sourceFz, computed, true, const(1))
    	return expandSubformula(sourceFz, script)
    }
    
    override def visitCommand(command: Command, computed: Script): Script = {
        command match {
            case x: CommandForceILoop => {
                // filter this command
                return computed :+ CommandAssert(
                        EQ(
                            Term.call(LTLInterpreter.loopEx),
                            Term.const(x.value)
                        )
                   )
            }
            case CommandDeclareTFun(name, args, returnType) => {
                // Converts temporal functions
                temporalFunctions.set(name, returnType)
                return computed :+ CommandDeclareFun(
                        name, domain::args, returnType)
            }
            case x: CommandAssert => {
                // Converts temporal asserts in SMT-lib style asserts
                return doExpandLTL(deneg(x.term), computed)
            }
            case x: CommandTemporalAssert => {
                // Converts temporal asserts in SMT-lib style asserts
                var script = computed
                var timeConst: SpecConstant = null
                x.time match {
                    case SpecIntConstant(time) => {
                        if (time > temporalExt) {
                            throw new IllegalArgumentException(
                                "Time must be <= " + temporalExt +
                                " found: " + time)
                        }
                        timeConst = const(time)
                    }
                    case SpecDoubleConstant(time) => {
                        if (time > temporalExt) {
                            throw new IllegalArgumentException(
                                "Time must be <= " + temporalExt +
                                " found: " + time)
                        }
                        timeConst = const(time.toInt)
                    }
                    case SpecStringConstant(time) => {
                        // Assert time is less the K
                        script = script :+
                        	CommandAssert(LE(TermConst(x.time),
                        	        Term.const(temporalExt)))
                       timeConst = x.time
                    }
                    case _ => {
                        throw new IllegalArgumentException(
                                "Time must be an int not: " + x.time)
                    }
                }
                val term = deneg(x.term)
                script = generateAllTemporalSupportFz(
                        term, computed, true, timeConst)
                script = expandSubformula(term, script) 
                return script
            }
            case x: CommandPush => {
                temporalFunctions = temporalFunctions.push()
                supportFzScope = supportFzScope.push()
                return computed :+ x
            }
            case x: CommandPop => {
                temporalFunctions.pop() match {
                    case Some(scope) => temporalFunctions = scope
                    case None => 
                        	throw new IllegalStateException(
                        	        "End of stack reached")
                }
                supportFzScope.pop() match {
                    case Some(scope) => supportFzScope = scope
                    case None => 
                        	throw new IllegalStateException(
                        	        "End of stack reached")
                }
                return computed :+ x
            }
            case x: Command => {
                return computed :+ x
            }
        }
    }
    
    
	/**
	 * Pushes negations to the atomic terms.
	 * This function is invoked recursively on all the sub functions.
	 * 
	 * This is a porting of the legacy deneg lisp function
	 * (defun deneg (f)
	 *	(declare (optimize (debug 0)(safety 0)(speed 3)))
	 *
	 *	(cond     
	 *		((null f) 'false)
	 *		((eq f t) 'true)   
	 *
	 *		((or (symbolp f) (stringp f) (integerp f) (predicatep f) ) f)
	 *		((eq (car f) 'not)
	 *			(let ((a (second f)))
	 *				(cond 
	 *					((eq a t) 'false)
	 *	 				((null a) 'true)	 
	 *					((or (symbolp a) (stringp a) (integerp a) (predicatep a)) f)	 
	 *	 				(t 
	 *	   					(case (car a)
	 *	     						((not) (deneg (second a)))
	 *	     						((and) (deneg (cons 'or (mapcar (lambda (x) (deneg `(not ,x))) (cdr a)))))
	 *	     						((or) (deneg (cons 'and (mapcar (lambda (x) (deneg `(not ,x))) (cdr a)))))
	 *	     						((next) `(next ,(deneg `(not ,(second a)))))
	 *	     						((until) `(release ,(deneg `(not ,(second a))) ,(deneg `(not ,(third a)))))
	 *	     						((release) `(until ,(deneg `(not ,(second a))) ,(deneg `(not ,(third a)))))
	 *	     						((yesterday) `(zeta ,(deneg `(not ,(second a)))))
	 *	     						((zeta) `(yesterday ,(deneg `(not ,(second a)))))
	 *								((since) `(trigger ,(deneg `(not ,(second a))) ,(deneg `(not ,(third a)))))	     
	 *	    						((trigger) `(since ,(deneg `(not ,(second a))) ,(deneg `(not ,(third a)))))
	 *	     						(t (error "deneg: bad arg ~S" (cons f a)))
	 *						)
	 *					)
	 *				)
	 *			)
	 *		) 
	 *		((and (consp f) (eq (car f) 'and) (null (cdr f))) 'true)
	 *    	((and (consp f) (eq (car f) 'or)  (null (cdr f))) 'false)
	 *		;((member (car f) '(and or next until release since trigger zeta yesterday))   
	 *   		(t (cons (car f) (mapcar #'deneg (cdr f))))
	 *	)
	 *)
	 *
	 **/
    def deneg(sourceFz: Term): Term = {
        sourceFz match {
            case x: ArithmeticOperator => {
                // The Deneg does not apply to arithmetic operators
                return x
            }
            case x: TermConst => {
                return x
            }       
            case x: TermQualIdentifierTerms => {
                
                return x
            }
            // Only boolean equalities have to be processed
            case x: EQ => {
                x.left match {
                    case Sort.Bool => {
                        // Boolean equality
                        return EQ(deneg(x.left), deneg(x.right))
                    }
                    case _ => {
                        // Arithmetic equality
                        return x
                    }
                }
            }
            // !true = false, !false = true
        	case Not(TermConst(SpecBooleanConstant(x))) => {
                return TermConst(SpecBooleanConstant(!x))
            }
        	// !4, !"foo" .... are this valid?
            case Not(TermConst(x)) => {
                return Not(TermConst(x))
            }
            // Double negation
            case Not(Not(x)) => {
                return deneg(x)
            }
            // De morgan's law
            case Not(x: And) => {
               return Or((for(op <- x.values) yield deneg(Not(op))): _*) 
            }
            case Not(x :Or) => {
               return And((for(op <- x.values) yield deneg(Not(op))): _*) 
            }
            // next: ((next) `(next ,(deneg `(not ,(second a)))))
            case Not(Next(x)) => {
                return Next(deneg(Not(x)))
            }
            // until: ((until) `(release ,(deneg `(not ,(second a))) ,(deneg `(not ,(third a)))))
            case Not(Until(x, y)) => {
                return Until(deneg(Not(x)), deneg(Not(y)))
            }
            // release: ((until) `(release ,(deneg `(not ,(second a))) ,(deneg `(not ,(third a)))))
            case Not(Release(x, y)) => {
                return Release(deneg(Not(x)), deneg(Not(y)))
            }
            // yesterday: ((yesterday) `(zeta ,(deneg `(not ,(second a)))))
            case Not(Yesterday(x)) => {
                return Zeta(deneg(Not(x)))
            }
            // zeta: ((zeta) `(yesterday ,(deneg `(not ,(second a)))))
            case Not(Zeta(x)) => {
                return Yesterday(deneg(Not(x)))
            }
            // since: ((since) `(trigger ,(deneg `(not ,(second a))) ,(deneg `(not ,(third a)))))
            case Not(Since(x, y)) => {
                return Trigger(deneg(Not(x)), deneg(Not(y)))
            }
            // trigger: ((trigger) `(since ,(deneg `(not ,(second a))) ,(deneg `(not ,(third a)))))
            case Not(Trigger(x, y)) => {
                return Since(deneg(Not(x)), deneg(Not(y)))
            }
            // explore all the sub 
            case x: UnaryOperator => {
				val clazz = x.getClass()
				val constructor = clazz.getConstructor(classOf[Term])
				return constructor.newInstance(deneg(x.value))
            }
            case x: BinaryOperator => {
                val clazz = x.getClass() 
				val constructor = clazz.getConstructor(
				        classOf[Term], classOf[Term])
				return constructor.newInstance(
				        deneg(x.value1), deneg(x.value2))
            }
            case x: TernaryOperator => {
                val clazz = x.getClass() 
				val constructor = clazz.getConstructor(
				        classOf[Term], classOf[Term], classOf[Term])
				return constructor.newInstance(
				        deneg(x.value1), deneg(x.value2), 
				        deneg(x.value3))
            }
            // TODO(m.sama): remove this
            // Read below for further explanation.
            case x: And => {
                return And({for (k <- x.values) yield deneg(k)}: _*)
            }
            case x: Or => {
                return Or({for (k <- x.values) yield deneg(k)}: _*)
            }
            // TODO(m.sama): the following gives an error.
            // The error is related to the implicit conversion between
            // scala's and java's varargs.
            // When this will work we can remove all the above MultiOperator
            case x: MultiOperator => {
                val clazz = x.getClass()
				val constructor = clazz.getConstructor(classOf[Seq[Term]])
				val args: Seq[Term] = {for (v <- x.values) yield deneg(v)}
				return constructor.newInstance(args: _*)
            }
        }
    }
}