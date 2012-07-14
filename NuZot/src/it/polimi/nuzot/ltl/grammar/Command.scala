/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.ltl.grammar

import it.polimi.nuzot.smt.grammar.{Command => SMTCommand}
import it.polimi.nuzot.smt.grammar.SpecConstant
import it.polimi.nuzot.smt.grammar.Symbol
import it.polimi.nuzot.smt.grammar.Sort
import it.polimi.nuzot.smt.grammar.Term


/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 *	<ltl_command> ::== <command>
 *		| ( force-i-loop <bool>)
 *		| ( assert-t <term> <spec_constant>)
 *      | ( declare-tfun <symbol> ( <sort>* ) <sort> )
 */
object Command {
    val forceILoop = "force-i-loop"
    val temporalAssert = "assert-t"
    val declareTFun = "declare-tfun"
}

sealed case class CommandForceILoop(val value: Boolean)
		extends SMTCommand {
    override def toString(): String = {
        return "(" + Command.forceILoop + "" + value + ")"
    }
}

/**
 * Define a time constrained assert.
 * The LTL proposition: (assert v t) can be represented
 * with in SMT as: (assert (v t)). 
 */
sealed case class CommandTemporalAssert(
        val term: Term, val time: SpecConstant)
		extends SMTCommand {
    
    override def toString(): String = {
        return "(" + Command.temporalAssert + " " + term +
        		" " + time + ")"
    }
}

sealed case class CommandDeclareTFun(val name: Symbol, 
        val args: List[Sort], val returnType: Sort) 
		extends SMTCommand {
    override def toString(): String = {
        return new StringBuilder("(" + Command.declareTFun + " ")
        	.append(name)
        	.append(" (")
        	.append(args.mkString(" "))
        	.append(") ")
        	.append(returnType)
        	.append(")")
        	.toString()
    }
}