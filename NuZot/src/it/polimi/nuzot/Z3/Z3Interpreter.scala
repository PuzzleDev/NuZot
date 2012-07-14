/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.Z3

import z3.scala._

import it.polimi.nuzot.core.DSLInterpreter
import it.polimi.nuzot.core.Scope
import it.polimi.nuzot.smt.grammar._


/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 * Uses the theorem solver Z3 to solve the loaded commands.
 */
class Z3Interpreter extends DSLInterpreter {
	val cfg = new Z3Config("MODEL" -> true)
	val z3 = new Z3Context(cfg)

	var stackDepth = 1
	
	var scopeFunctions = new Scope[Symbol, Z3FuncDecl]()
	var scopeSorts = new Scope[Symbol, Z3Sort]()
	scopeSorts.set(Symbol.Bool, z3.mkBoolSort())
	scopeSorts.set(Symbol.Int, z3.mkIntSort())
	scopeSorts.set(Symbol.Real, z3.mkRealSort())
	
	var scopeSortedVar = new Scope[Symbol, Z3AST]()
    
	override def generatePreconditions(): Script = {
	    return new Script()
	}
	
	private def visitSort(sort: Sort): Z3Sort = {
	    sort match {
		    case SortIdentifier(IdentifierSymbol(x)) => {
		        scopeSorts.get(x) match {
		            case Some(scopeSort) => return scopeSort
		            case None => 
		                throw new IllegalStateException("Undefined: " + x)
		        }
		    }
		    case _ => {
		        // TODO(m.sama): not implemented
		        throw new IllegalArgumentException(
		                "Not implemented yet!")
		    }
		}
	}
	
	def visitKnownFunctions(term: Term): Z3AST = {
	    term match {
            case y: Add => {
                return z3.mkAdd(
                        {for (k <- y.values) yield visitTerm(k)}: _*)
            }
            case y: And => {
                return z3.mkAnd(
                         {for (k <- y.values) yield visitTerm(k)}: _*)
            }
            case y: Div => {
                return z3.mkDiv(
                        visitTerm(y.nom), 
                        visitTerm(y.denom))
            }
            case y: Mod => {
                return z3.mkMod(
                        visitTerm(y.nom),
                        visitTerm(y.denom))
            }
            case y: Rem => {
                return z3.mkRem(
                        visitTerm(y.left),
                        visitTerm(y.right))
            }
            case y: EQ => {
                return z3.mkEq(
                        visitTerm(y.left),
                        visitTerm(y.right))
            }
            case y: GE => {
                return z3.mkGE(
                        visitTerm(y.left),
                        visitTerm(y.right))
            }
            case y: GT => {
                return z3.mkGT(
                        visitTerm(y.left),
                        visitTerm(y.right))
            }
            case y: ITE => {
                return z3.mkITE(
                        visitTerm(y.ifa),
                        visitTerm(y.thena),
                        visitTerm(y.elsea))
            }
            case y: IFF => {
                return z3.mkIff(
                        visitTerm(y.ifa),
                        visitTerm(y.thena))
            }
            case y: IMP => {
                return z3.mkImplies(
                        visitTerm(y.ifa),
                        visitTerm(y.thena))
            }
            case y: LE => {
                return z3.mkLE(
                        visitTerm(y.left),
                        visitTerm(y.right))
            }
            case y: LT => {
                return z3.mkLT(
                        visitTerm(y.left), 
                        visitTerm(y.right))
            }
            case y: Mul => {
                return z3.mkMul(
                        {for (k <- y.values) yield visitTerm(k)}: _*)
            }
            case y: Not => {
                return z3.mkNot(
                        visitTerm(y.value))
            }
            case y: Or => {
                return z3.mkOr(
                         {for (k <- y.values) yield visitTerm(k)}: _*)
            }
            case y: Sub => {
                return z3.mkSub(
                         {for (k <- y.values) yield visitTerm(k)}: _*)
            }
            case y: Xor => {
                return z3.mkXor(
                        visitTerm(y.left),
                        visitTerm(y.right))
            }
            case _ => {
                throw new IllegalStateException(
                        "Unrecognized operation: " + term)
            }
        }
	}
	
	def visitTerm(term: Term): Z3AST = {
	    term match {
            case TermConst(x) => {
                x match {
                    case SpecBooleanConstant(y) => {
                        return if (y) {
                        	z3.mkTrue()    
                        } else {
                        	z3.mkFalse()
                        }
                    }
                    case SpecIntConstant(y) => {
                        return z3.mkInt(y, z3.mkIntSort())
                    }
                    case SpecDoubleConstant(y)  => {
                        // XXX(michele.sama): the current version of
                        // ScalaZ3 does not support Real constants.
                        // This will approximate a real to a fraction
                        val precision: Int = 10000000
                        return z3.mkReal((y * precision).toInt, precision)
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
                val sortedVar = scopeSortedVar.get(symbol)
                sortedVar match {
                    case Some(v) => return v
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
	    val z3Name = z3.mkStringSymbol(name.name)
	    val z3Params: List[Z3Sort] = for (x <- args) yield visitSort(x)
	    val z3ReturnType: Z3Sort = visitSort(returnType) 
	    
	    z3Params match {
	        case List() => {
	            val const = z3.mkConst(z3Name, z3ReturnType)
	            scopeSortedVar.set(name, const)
	        }
	        case x::Nil => {
	            val funDecl = z3.mkFuncDecl(z3Name, z3Params.head, z3ReturnType)
	            scopeFunctions.set(name, funDecl)
	        }
	        case x::y => {
	            val funDecl = z3.mkFuncDecl(z3Name, z3Params, z3ReturnType)
	            scopeFunctions.set(name, funDecl)
	        }
	    }
	}
	
	private def checkArgsLength(
	        name: String, size: Int, args: Seq[Z3AST]) = {
	    if (args.length != size) {
            throw new IllegalArgumentException(
                    "function " + name + 
                    " wants " + size + 
                    " arguments. Found: " + args)
        }
	}
	
	/**
	 * Commodity function used to check that a given multi-operator
	 * would be invoked with a minimum number of arguments. 
	 * 
	 * <p>For instance and AND should have at least 2 arguments.
	 */
	private def checkArgsAtLeast(
	        name: String, size: Int, args: Seq[Z3AST]) = {
	    if (args.length < size) {
            throw new IllegalArgumentException(
                    "function " + name + 
                    " wants at least" + size + 
                    " arguments. Found: " + args)
        }
	}
	
	private def visitFunctionInvocation(
	        fzName: QualIdentifier, args: Seq[Z3AST]): Z3AST = {
	    fzName match {
	        case QualIdentifierIdentifier(IdentifierSymbol(symbol)) => {
			    scopeFunctions.get(symbol) match {
			        case Some(fn) => {
			            checkArgsLength(fzName.toString(), fn.arity, args)
			            return z3.mkApp(fn, args: _*)
			        }
			        case None => {
			            throw new IllegalArgumentException(
			                    "Undefined function: " + fzName)
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
	    var sort: Z3Sort = null
	    value match {
	        case x: Boolean => {
	            sort = z3.mkBoolSort()
	        }
	        case x: Int => {
	            sort = z3.mkIntSort()
	        }
	        case x: Float => {
	            sort = z3.mkRealSort()
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
            return computed
        }
        // TODO(m.sama): use log.verbose instead of this
        println(command)
        command match {
            case x: CommandExit => {
                z3.delete()
                terminated(true)
            }
            case x: CommandGetModel => {
                z3.checkAndGetModel match {
			        case (None, _) => println("Z3 failed. The reason is: " +
			                z3.getSearchFailure.message)
			        case (Some(false), _) => println("UNSAT")
			        case (Some(true), model) => {
			            println("Model:")
			            println(model)
			            model.delete
			        }
                }
            }
            case x: CommandCheckSat => {
                println(if (checkSat()) "SAT" else "UNSAT")
            }
            case x: CommandPush => {
                z3.push()
                stackDepth += 1
                println("Stack level: " + stackDepth)
                // Update local stack
                scopeFunctions = scopeFunctions.push()
                scopeSorts = scopeSorts.push()
                scopeSortedVar = scopeSortedVar.push()
            }
            case x: CommandPop => {
                if (stackDepth <= 1) {
                    println("End of stack reached: skipping")
                    return computed
                } else {
                    stackDepth -= 1
                    println("Stack level: " + stackDepth)
                }
                z3.pop(1)
                // Clear stack
                scopeFunctions.pop() match {
                    case Some(scope) => scopeFunctions = scope
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
                scopeSortedVar.pop() match {
                    case Some(scope) => scopeSortedVar = scope
                    case None => 
                        	throw new IllegalStateException(
                        	        "End of stack reached")
                }
            }
            case x: CommandDeclareFun => 
            	visitCommandDeclareFun(x.name, x.args, x.returnType)
            case x: CommandDefineFun => {
            	// val intSort = z3.mkIntSort
            	// declares a function symbol of type int->int
            	// val fibonacci = z3.mkFreshFuncDecl("fib", List(intSort), intSort)
                // TODO(m.sama) add the defined function to a local stack
                
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
                z3.assertCnstr(visitTerm(x.term))
            }
            case x: InitCommandSetInfo => {
                // Set info is used internally to
                // configure the interpreter
            }
            case x: InitCommandSetLogic => {
                // Set logic is used internally to
		        // configure the interpreter
            }
            case x: CommandEcho => {
                println(x.msg.substring(1,  x.msg.length() - 1))
            }
            case _ => {
                println("Unknown command: " + command)
            }
        }
        return computed
    } 
    
    /**
     * Check if the script interpreted so far is
     * satisfied or not.
     * 
     * @return <code>true</code> if the script has a solution,
     * 		<code>false</code> otherwise.
     */
    def checkSat(): Boolean = {
        if (terminated()) {
            throw new IllegalStateException(
                    "Cannot check sat after the " +
                    "program has been terminated.")
        }
        
		return z3.check() match {
		    case Some(x) => return x
		    case None => return false
		}
    }
    
    
    def getModel(): Z3Model = {
        z3.checkAndGetModel match {
	        case (None, _) => {
	            throw new IllegalStateException(z3.getSearchFailure.message)
	        }
	        case (Some(false), _) => {
	            throw new IllegalStateException("Unsat")
	        }
	        case (Some(true), model) => {
	            return model
	        }
        }
    }
        
}