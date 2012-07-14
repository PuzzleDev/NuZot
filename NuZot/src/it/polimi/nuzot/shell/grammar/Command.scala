/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.shell.grammar

import it.polimi.nuzot.smt.grammar.{Command => SMTCommand}

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 *	<shell_command> ::== <command>
 *		| ( save <string>)
 *		| ( load <string>)
 */
object Command {
    val loadLabel = "load"
    val saveLabel = "save"

    
	def save(filename: String): SMTCommand = {
    	return new CommandSave(filename)
    }
	
	def load(filename: String): SMTCommand = {
    	return new CommandLoad(filename)
    }
}

sealed case class CommandSave(val filename: String) extends SMTCommand {
    override def toString(): String = {
        return "(" + Command.saveLabel + " " + filename + ")"
    }
}

sealed case class CommandLoad(val filename: String) extends SMTCommand {
    override def toString(): String = {
        return "(" + Command.loadLabel +" " + filename + ")"
    }
}