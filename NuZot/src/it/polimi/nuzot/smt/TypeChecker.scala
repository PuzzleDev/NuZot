/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.smt

import it.polimi.nuzot.core._
import it.polimi.nuzot.smt.grammar._

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 * Perform a strong type checking on function invocation
 */
object TypeChecker {
    val domainKeyword = Keyword(":domain")
}

class TypeChecker extends DSLInterpreter {
	
    var scopeFunctionArgs = new Scope[Symbol, List[Sort]]()
	
    var scopeFunctionReturnTypes = new Scope[Symbol, Sort]()
	
	var scopeSorts = new Scope[Symbol, Sort]()
	scopeSorts.set(Symbol.Bool, Sort.Bool)
	// Other sort will be added according to the current domain
	
	var domain: Sort = Sort.Int
	
	override protected def initializeAttributes(): Unit = {
        // Set the number domain
        attributes.get(TypeChecker.domainKeyword) match {
            case Some(AttributeKeyVal(k, AttributeValueSymbol(Symbol.Int))) => {
                domain = Sort.Int
                scopeSorts.set(Symbol.Int, Sort.Int)
            }
            case Some(AttributeKeyVal(k, AttributeValueSymbol(Symbol.Real))) => {
                domain = Sort.Real
                scopeSorts.set(Symbol.Real, Sort.Real)
            }
            case Some(x) => {
                throw new IllegalArgumentException(
                        "Invalid " + TypeChecker.domainKeyword + ": " + x + ".")
            }
            case None => {
                // Using default
            }
        }
    }
    
    override protected def initializeDelegates(): Unit = {}
	
	override def generatePreconditions(): Script = {
	    return new Script()
	}
	
	private def assertSame(operator: Term, expected: Sort, found: Sort): Unit = {
	    if (expected != found) {
	        throw new IllegalArgumentException(
	                operator + "Wrong type. Expected: " + expected + 
	                ", found: " + found)
	    }
	}
	
	def assertKnownFunctions(operator: Term, expected: Sort) {
	    operator match {
	        case y: UnaryOperator => {
	            assertSame(y.value, expected, visitTerm(y.value))
	        }
	        case y: BinaryOperator => {
	            assertSame(y, expected, visitTerm(y.value1))
	            assertSame(y, expected, visitTerm(y.value2))
	        }
	        case y: TernaryOperator => {
	            assertSame(y, expected, visitTerm(y.value1))
	            assertSame(y, expected, visitTerm(y.value2))
	            assertSame(y, expected, visitTerm(y.value3))
	        }
	        case y: MultiOperator => {
	            for (value <- y.values) {
	                assertSame(value, expected, visitTerm(value))
	            }
	        }
	    }	       
	}
	
	def visitKnownFunctions(term: Term): Sort = {
	    term match {
	        // EQ behaves differently
	        // TODO(m.sama): it would be better to have
	        // 		an arithmetic and a boolean equality
	        case EQ(x, y) => {
	            // TODO(michele.sama): assert x, y in [domain, bool]
	            assertSame(term, visitTerm(x), visitTerm(y))
	            return Sort.Bool
	        }
	        case x: EqualityOperator => {
	            assertSame(x, visitTerm(x.value1), domain)
	            assertSame(x, visitTerm(x.value2), domain)
	            return Sort.Bool
	        }
	        case x: ArithmeticOperator => {
	            assertKnownFunctions(x, domain)
	            return domain
	        }
	        case x: BooleanOperator => {
	            val sort = Sort.Bool
	            assertKnownFunctions(x, sort)
	            return sort
	        }
            case _ => {
                throw new IllegalStateException(
                        "Unrecognized operation: " + term)
            }
        }
	}
	
	def visitTerm(term: Term): Sort = {
	    term match {
            case TermConst(x) => {
                x match {
                    case SpecBooleanConstant(y) => {
                        return Sort.Bool
                    }
                    case SpecIntConstant(y) => {
                        return Sort.Int
                    }
                    case SpecDoubleConstant(y)  => {
                        return Sort.Real
                    }
                    //MR: added as a try
                    case SpecTermConstant(y)  => {
                        return visitTerm(y)
                    }

                    case _ => 
                        // TODO(m.sama): not implemented
				        throw new IllegalArgumentException(
				                "Not implemented yet!")
                }
            }
            // known operators
            case x: KnownFunctionInvocation => {
                return visitKnownFunctions(x)
            }
            // A function with arity = 0, e.g. a constant
            case TermQualIdentifierTerms(
                    QualIdentifierIdentifier(
                            IdentifierSymbol(symbol))) => {
                // Function name or variable
                val returnType = scopeFunctionReturnTypes.get(symbol)
                returnType match {
                    case Some(v) => {
                        return v
                    }
                    case None => throw new IllegalStateException(
                            "Not found in scope: " + symbol)
                }
            }
            // A parametric function
            case x: TermQualIdentifierTerms => {
                // Function invocation
                val fnName = x.qualIdentifier
                val args = for {arg <- x.terms} yield (visitTerm(arg))
                return visitFunctionInvocation(fnName, args)
            }
            case x: Term => {
                throw new IllegalStateException("Not supported: " + x)
            }
	    }
	}
	
	private def visitCommandDeclareFun(name: Symbol, 
			args: List[Sort], returnType: Sort): Unit = {
	    scopeFunctionArgs.set(name, args)
	    scopeFunctionReturnTypes.set(name, returnType)
	}
	

	private def checkArgs(
	        name: String, paramDef: List[Sort], paramAss: Seq[Sort]) = {
	    // XXX(m.sama): is this checking the whole list?
	    if (paramDef != paramAss) {
	        throw new IllegalArgumentException(
	                "Wrong param in " + name + ". Expected: " + paramDef +
	                " found: " + paramAss)
	    }
	}
	
	private def visitFunctionInvocation(
	        fzName: QualIdentifier, args: Seq[Sort]): Sort = {
	    fzName match {
	        case QualIdentifierIdentifier(IdentifierSymbol(symbol)) => {
			    scopeFunctionArgs.get(symbol) match {
			        case Some(defArgs) => {
			            checkArgs(fzName.toString(), defArgs, args)
			        }
			        case None => {
			            throw new IllegalArgumentException(
			                    "Not found: " + symbol)
			        }
			    }
			    return scopeFunctionReturnTypes.get(symbol) match {
			        case Some(s) => return s
			        case None => {
			            throw new IllegalArgumentException(
			                    "Not found: " + symbol)
			        }
			    }
	        }
	        case _ => {
	        	// TODO(m.sama): not implemented
		        throw new IllegalArgumentException(
		                "Not implemented yet!")

	        }
	    }
	}
	
	private def visitCommandDeclareSort(name: Symbol, value: AnyVal): Unit = {
	    var sort: Sort = null
	    value match {
	        case x: Boolean => {
	            sort = Sort.Bool
	        }
	        case x: Int => {
	            sort = Sort.Int
	        }
	        case x: Float => {
	            sort = Sort.Real
	        }
	        case _ => {
	            throw new IllegalStateException("Unexpected sort type: " + value)
	        }
	    }
	    // Add to local stack
	    scopeSorts.set(name, sort)
	}
	
	override protected def visitCommand(
	        command: Command, computed: Script): Script = {
        // If terminated skip all the further commands
        if (terminated) {
            return computed :+ command
        }

        command match {
            case x: CommandPush => {
                // Update local stack
                scopeFunctionArgs = scopeFunctionArgs.push()
                scopeFunctionReturnTypes = scopeFunctionReturnTypes.push()
                scopeSorts = scopeSorts.push()
            }
            case x: CommandPop => {
                // Clear stack
                scopeFunctionArgs.pop() match {
                    case Some(scope) => scopeFunctionArgs = scope
                    case None => 
                        	throw new IllegalStateException(
                        	        "End of stack reached")
                }
                scopeFunctionReturnTypes.pop() match {
                    case Some(scope) => scopeFunctionReturnTypes = scope
                    case None => 
                        	throw new IllegalStateException(
                        	        "End of stack reached")
                }
                scopeSorts.pop() match {
                    case Some(scope) => scopeSorts = scope
                    case None => 
                        	throw new IllegalStateException(
                        	        "End of stack reached")
                }
            }
            case x: CommandDeclareFun => 
                // Check the return type of declared functions
                // The return type should be the same as the domain
                if (x.returnType != domain && x.returnType != Sort.Bool) {
                    throw new IllegalStateException(
                        	"Wrong return type found: " + x.returnType + 
                        	" expected:" + domain)
                }
            	visitCommandDeclareFun(x.name, x.args, x.returnType)
            case x: CommandDefineFun => {
            	// TODO(m.sama): not implemented
		        throw new IllegalArgumentException(
		                "Not implemented yet!")

            }
            case x: CommandDefineSort => {
                // TODO(m.sama): not implemented
		        throw new IllegalArgumentException(
		                "Not implemented yet!")

            }
            case x: CommandDeclareSort => 
            	visitCommandDeclareSort(x.symbol, x.value)
            case x: CommandAssert => {
                visitTerm(x.term)
            }
            case _ => {}
        }
        return computed :+ command
    } 
}