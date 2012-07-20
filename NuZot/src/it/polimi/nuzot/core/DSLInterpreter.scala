/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.core

import scala.util.parsing.combinator.JavaTokenParsers

import it.polimi.nuzot.smt.grammar._
import it.polimi.nuzot.shell.grammar.ShellCommand


/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 * Creates a chain of responsibility with a set of visitors.
 * The last ring of the chain should either invoke a solver or
 * compute the script. The previous rings should pre-compute the
 * script and pass the result to their follower.
 */
trait DSLInterpreter {

    // The next interpreter in the chain.
    private var _next: DSLInterpreter = null
    
    // The list of attributes
    var attributes: Map[Keyword, Attribute] = Map()
	
    // The list of logics
    // Logics are used to load delegates.
    var logics: List[Symbol] = List()
    var delegateClasses: List[DSLInterpreterDelegate[_]] = List()

    // The script as it is passed to this
    // interpreter.
    var original: Script = new Script();
    
    // Defines is this interpreter is still
    // initializng or not. Initialing interpreter
    // can process InitCommands.
    private var _initializing = true
    
    // Defines if the execution of this interpreter is
    // terminated. The execution terminates when the
    // CommandExit is interpreted.
    private var _terminated: Boolean = false
	

	/**
     * The next interpreter in the chain.
     * If <code>null</code> this interpreter is the
     * one which should handle the computation.
     */
	def next(): DSLInterpreter = _next
	
	def next(value:DSLInterpreter): DSLInterpreter = {
		_next = value
		return this
	} 
    
    /**
     * Get the initialization status of this interpreter.
     * 
     * @return <code>true</code> if this interpreter is 
     * 		still initializing, <code>false</code> otherwise.
     */
    def isInitializing(): Boolean = {
        return _initializing
    }
		
    /**
     * Append a command to the list of
     * executed command.
     * 
     * @param command the given command to add to
     * 		list of script.
     * @return the new script.
     */
    private def append(command: Command): Script = {
        original = original :+ command
        return original
    }
	
    protected def initializeAttributes(): Unit;
    
    protected def initializeDelegates(): Unit;
    
	/**
	 * Initializes the interpreter.
	 * This method is automatically invoked after all 
	 * the parameters are loaded.
	 */
	protected def generatePreconditions(): Script;
	
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
	
	/**
	 * Visit a command. This is invoked automatically
	 * from innerVisitCommand.
	 * 
	 * Derived classes should override this method with their
	 * implementation.
	 */
	protected def visitCommand(
	        command: Command, computed: Script): Script = {
	    return computed :+ command 
	}
	
	/**
	 * Visit a command. Internally invokes the visitCommand
	 * method which will be redefined in each derived class.
	 * 
	 * @param command the command to be processed.
	 * @param computed the list of computed commands.
	 * @return a new script containing the computed script
	 * 		plus the last executed command.
	 */
	final private def innerVisitCommand(
	        command: Command, computed: Script): Script = {
        command match {
            case initCommand: InitCommand => {
                // Throws an error if the interpreter is not
                // initializing anymore.
                if (_initializing == false) {
                    throw new IllegalStateException(
                            "Initialization commands can only be interpreted " +
                            "at the beginning of the script")
                }
                initCommand match {
	                case x: InitCommandSetLogic => {
		                logics = logics :+ x.symbol
		                val clazz = Class.forName(x.symbol.toString())
		                val instance = clazz.newInstance()
		                instance match {
		                    case delegate: DSLInterpreterDelegate[DSLInterpreter] => {
		                        delegateClasses = delegateClasses :+ delegate
		                    }
		                    case _ => {
		                        throw new IllegalArgumentException(
		                                "Not a delegate: " + instance)
		                    }
		                }
		            }
		            case x: InitCommandSetInfo => {
		                attributes = 
		                    attributes ++: Map(x.attribute.keyword -> x.attribute)
		            }                    
                }
		        return visitCommand(initCommand, computed)
            }
            case shellCommand: ShellCommand => {
                // Shell command are treated differently than other commands
                // This allows you to load a script with  its 
                // initializations inside.
                return visitCommand(shellCommand, computed)
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
	                        initializeDelegates()
	                        initializeAttributes()
	                        script = script ++ generatePreconditions()
	                    }
	                }
                }
                return visitCommand(com, script)
            }
        }
    }
	
	private final def visitScript(
	        source: Script, computed: Script): Script = {
        source.commands match {
            case Nil => return computed
            case head::tail => {
                try {
                    append(head)
                    val script = visitScript(Script(tail),
                    		innerVisitCommand(head, computed))
                    
                    return script
                } catch {
                    case ex: RuntimeException => {
                        original = original.previous()
                        println("An error has occurred: "+ ex.getMessage())
                        ex.printStackTrace()
                        return computed
                    }
                }
            }
        }
    }
	
	/**
     * Visits the script with the current visitor and
     * forwards the modified DOM to the next visitor. 
     */
    final def doVisit(source: Script): Script = {
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
    
    /**
     * Forces a push to the current interpreter.
     */
    def push(): Unit = {
    	doVisit(Script(CommandPush() :: List()))
    }
    
    /**
     * Forces a pop to the current interpreter.
     */
    def pop(): Unit = {
        doVisit(Script(CommandPop() :: List()))
    }
}