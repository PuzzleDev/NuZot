/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.shell

import java.io._
import it.polimi.nuzot.core.DSLInterpreter
import it.polimi.nuzot.smt.grammar._
import it.polimi.nuzot.shell.grammar._
import scala.io.Source


/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 * Executes all the shell commands and forwards
 * all the resulting instructions to the next 
 * interpreter in the chain.
 * 
 * <p>Interpreters are implemented as a state-full
 * chain-of-responsibility pattern.
 * 
 * Shell commands are:
 * <li><b>save</b>: saves all the commands until the save command.
 * The save commands are not saved.
 * <li><b>load</b>: loads a previously saved script. 
 */
class ShellInterpreter extends DSLInterpreter {

    // still interpreting init commands
    var _initializing = true
    private def initializationDone(): Unit = 
        	_initializing = false
    
    def generatePreconditions(): Script = {
        new Script()
    }
    
    override def visitCommand(command: Command, computed: Script): Script = {
        command match {
            case x: CommandSave => {
                val filename = x.filename.substring(1,  x.filename.length() - 1)
                innerSave(filename, original ++ computed)
                return computed
            }
            case x: CommandLoad => {
                val filename = x.filename.substring(1,  x.filename.length() - 1)
                return computed ++ innerLoad(filename)
            }
            case x: CommandExit => {
                terminated(true)
                return computed :+ x
            }
            case x: Command => {
                return computed :+ x
            }
        }
    }
    
    def load(name: String): Unit = {
    	doVisit(Script(CommandLoad("\"" + name + "\"") :: List()))
    }
    
    /**
     * @param filename the file name.
     * @return the loaded script or an empty script 
     * 		if an error occurred.
     */
    def innerLoad(filename: String): Script = {
        try {
            val parser = new ShellParser()
            return parser.loadFile(filename)
        } catch {
            case e: Exception => println(e.getMessage())
            return new Script()
        }
    }
    
    def save(name: String): Unit = {
    	doVisit(Script(CommandSave("\"" + name + "\"") :: List()))
    }

    def innerSave(filename: String, script: Script): Unit = {
        try {
            val writer = new PrintWriter(filename)
            writer.write(script.toString())
            writer.flush()
            writer.close()
            println("Script saved succesfully.")
        } catch {
            case e: Exception => println(e.getMessage())
        }
    }
}