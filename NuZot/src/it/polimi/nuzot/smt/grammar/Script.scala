/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.smt.grammar
import scala.collection.immutable.Queue

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 * <script> ::== <command>*
 */
case class Script(val commands: List[Command]) extends Statement {
    
    def this() = this(List())
    
    override def toString(): String = {
        commands.mkString("\n")
    }
    
    def :+(command: Command): Script = {
        command match {
            case null => {
                return this
            }
            case x => {
                return Script(commands :+ command)
            }
        }
    }
    
    def ++(script: Script): Script = {
        script match {
            case null => {
                return this
            }
            case x => {
                return Script(commands ++ script.commands)
            }
        }
    }
}