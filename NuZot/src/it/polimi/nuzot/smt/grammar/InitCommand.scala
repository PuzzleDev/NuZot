/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.smt.grammar

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 * Defines all the initialization commands.
 * 
 * <p>Initialization commands can only be applied at the
 * beginning of the execution and are used to configure
 * how the script will be interpreted.
 * 
 * <init_command> ::== ( set-logic <symbol> )
 *		| ( set-info <attribute> )
 */
trait InitCommand extends Command

sealed case class InitCommandSetLogic(val symbol: Symbol) extends InitCommand {
    override def toString(): String = {
        return "(" + Command.setLogic + " " + symbol + ")"
    }
}

sealed case class InitCommandSetInfo(val attribute: Attribute) extends InitCommand {
    override def toString(): String = {
        return "(set-info " + attribute + ")"
    }
}