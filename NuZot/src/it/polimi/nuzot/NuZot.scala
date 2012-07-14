/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot

import java.io.FileReader
import scala.collection.mutable.Stack
import it.polimi.nuzot.shell.ShellParser
import it.polimi.nuzot.shell.ShellInterpreter
import it.polimi.nuzot.Z3.Z3Interpreter
import it.polimi.nuzot.ltl.LTLInterpreter
import it.polimi.nuzot.smt.grammar.Script
import it.polimi.nuzot.smt.TypeChecker


/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 * TODO(m.sama): find a cool way to add 
 * 		interpreter dependent params.
 */
object NuZot {
    
    val paramFile = "--file";
    val paramShell = "--shell";
    
	def main(args: Array[String]) {
		if (args.length == 0) {
			printUsage()
		}
		val params = parseParams(Map[String, String](), args.toList)
		val nuZot = new NuZot(params, args)
		nuZot.doCompute()
    }
	
	/**
	 * Prints the standard usage message.
	 */
	def printUsage(): Unit = {
	    println("Welcome to NuZot")
	    println("Usage: NuZot [" + paramFile + 
	           " <filename>] [" + paramShell + "]")
	    println("If both file and shell mode are active the shell " +
	    		"commands are interpreted only after the file has " +
	    		"been processed.")
	}
	
	/**
	 * Recursively loads all the parameters.
	 * Parameters with argument are loaded as 
	 * <code>Map(command -> arg)</code>, parameter without arguments
	 * are loaded as <code>Map(command -> "")</code>
	 * 
	 * @param map a command-> arg map.
	 * @param args the tail of the command list.
	 * @return a map containing the original map plus 
	 * 		the parameter parsed in this iteration.
	 */
	def parseParams(map: Map[String, String], args: List[String]):
			Map[String, String] = {
	    args match {
	        case Nil => map
	        case paramFile :: head :: tail => {
	            parseParams(map ++ Map(paramFile -> head), tail)
	        }
	        case paramShell :: tail => {
	            parseParams(map ++ Map(paramShell -> ""), tail)
	        }
	    }
	}
	
}

class NuZot(val params:Map[String, String], val args: Array[String]) {
    val interpreter = new ShellInterpreter()
    		.next(new LTLInterpreter()
    			.next(new TypeChecker()
    				.next(new Z3Interpreter())))
    			
    def doCompute(): Unit = {
    	params.get(NuZot.paramFile) match {
		    case Some(fileName) => {
		         doParseFile(fileName)
		    }
		    case None => {}
		}
	
		params.get(NuZot.paramShell) match {
		    case Some(x) => {
		        doParseShell()
		    }
		    case None => {}
		}
		
    }
    
    def doParseFile(fileName: String): Unit = {
        println("Processing file: " + fileName)
        val parser = new ShellParser()
        val script = parser.loadFile(fileName)
        if (script != null) {
        	interpreter.doVisit(script)
        	println("File: " + fileName + " processed succesfully!")
        } else {
            println("Parse error")
        }
    }
    
    /**
     * Starts the shell mode, reading line by line
     * interactive SMT-lib style commands.
     * 
     * The shell terminates when the user types 
     * the (exit) command.
     */
    def doParseShell(): Unit = {
        println("Welcome to NuZot shell.")
		println("Type '(exit)' to leave.")
		println("Type '(save \"filename\")' to save.")
		println("Type '(load \"filename\")' to load a script at the current stack level.")
		
		if (interpreter.terminated) {
            println("The shell is skipped because " +
            		"an (exit) instruction has already " +
            		"been processed.")
            return
        }
		
	    val builder = new StringBuilder()
	    while (!interpreter.terminated) {
	        if (builder.length == 0) {
	        	print("NuZot>")
	        } else {
	            print("NuZot>... ")
	        }
	        val line = builder.append(readLine()).toString()
	        if (line.count(x => x == '(') == line.count(x => x == ')')) {
	            // The command is complete
	            val parser = new ShellParser()
		        val script = parser.parseAll(
		                parser.script, line)
		                .getOrElse(null)
		        builder.clear()
		        if (script != null) {
	            	interpreter.doVisit(script)
		        } else {
		            println("Wrong command: skipping... " + line)
		        }
	        }
	    }
        println("NuZot shell terminated")
	}
}
