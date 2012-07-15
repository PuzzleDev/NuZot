/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.core

import it.polimi.nuzot.smt.grammar._
import scala.util.parsing.combinator.JavaTokenParsers

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 * Creates a chain of responsibility with a set of visitors.
 * The last ring of the chain should either invoke a solver or
 * compute the script. The previous rings should pre-compute the
 * script and pass the result to their follower.
 */
trait DSLInterpreter {

	/**
     * The next interpreter in the chain.
     * If <code>null</code> this interpreter is the
     * one which should handle the computation.
     */
	def next = _next
	
	def next(value:DSLInterpreter): DSLInterpreter = {
		_next = value
		return this
	} 
	private var _next: DSLInterpreter = null
	
	var attributes: Map[Keyword, Attribute] = Map()
	var logics: List[Symbol] = List()
	
	var original: Script = new Script();
    
    private def append(command: Command): Script = {
        original = original :+ command
        return original
    }
    
    private def append(script: Script): Script = {
        original = original ++ script
        return original
    }
	
	/**
	 * Initializes the interpreter.
	 * This method is automatically invoked after all 
	 * the parameters are loaded.
	 */
	protected def generatePreconditions() : Script;
	
	/**
	 * State that the computation is terminated.
	 * This can happen in case of a blocking exception
	 * or when an exit instruction is processed.
	 * 
	 * When one of the rings terminates the whole
	 * chain terminates as well.
	 */
	def terminated(): Boolean = {
	    if (_terminated) {
	    	return true
	    } else {
		    next match {
			    case x: DSLInterpreter => {
			        return next.terminated()
			    }
			    case null => {
			        return _terminated
			    }
			}   
	    }
	}
	
	/**
	 * Set the terminate status of this ring.
	 */
	def terminated(value: Boolean): Unit = _terminated = value
	private var _terminated: Boolean = false
	
	private var _initializing = true
	
	protected def visitCommand(command: Command, computed: Script): Script = {
	    return computed :+ command 
	}
	
	final private def innerVisitCommand(command: Command, computed: Script): Script = {
        command match {
            case initCommand: InitCommand => {
                // Throws an error if the interpreter is not
                // initializing anymore.
                if (_initializing == false) {
                    throw new IllegalStateException(
                            "Initialization commands can only be interpreted " +
                            "at the beginnin of the script")
                }
                initCommand match {
	                case x: InitCommandSetLogic => {
		                logics = 
		                    logics :+ x.symbol
		            }
		            case x: InitCommandSetInfo => {
		                attributes = 
		                    attributes ++: Map(x.attribute.keyword -> x.attribute)
		            }                    
                }
		        return visitCommand(initCommand, computed)
            }
            
            case com: Command => {
                // After the first command the system
                // is not initializing anymore
                var script = computed

                if (_initializing) {
                	_initializing = false
                	com match {
	                    case x: CommandExit => {
	                        // The first instruction is an (exit)
	                    	// Skip initialization
	                    }
	                    case _ => {
	                        script = script ++ generatePreconditions()
	                    }
	                }
                }
                return visitCommand(com, script)
            }
        }
    }
	
	protected def visitScript(source: Script, computed: Script): Script = {
        source.commands match {
            case Nil => return computed
            case head::tail => {
                return visitScript(Script(tail),
                        innerVisitCommand(head, computed))
            }
        }
    }
	
	/**
     * Visits the script with the current visitor and
     * forwards the modified DOM to the next visitor. 
     */
    final def doVisit(source: Script): Script = {
        append(source)
		val computedScript = visitScript(source, new Script())
		next match {
		    case x: DSLInterpreter => {
		        next.doVisit(computedScript)
		    }
		    case null => {
		        // The next visitor is null.
		        // This ring is the last of the chain.
		    }
		}
		return computedScript
    }
    
    def push(): Unit = {
        doVisit(Script(CommandPush() :: List()))
    }
    
    def pop(): Unit = {
        doVisit(Script(CommandPop() :: List()))
    }
}