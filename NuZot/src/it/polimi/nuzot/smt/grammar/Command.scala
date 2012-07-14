/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.smt.grammar

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 * 
 * <command> ::== ( assert <term> )
 *		| ( check-sat )
 *		| ( get-model )
 *      | ( declare-const <symbol> <sort> ) // NOT SMT-lib
 *		| ( declare-sort <symbol> <numeral> )
 *		| ( define-sort <symbol> ( <symbol>* ) <sort> )
 *		| ( declare-fun <symbol> ( <sort>* ) <sort> )
 *		| ( define-fun <symbol> ( <sorted_var>* ) <sort> <term> )
 *		| ( exit )
 *      | ( echo <string> )
 *		| ( push )
 *		| ( pop )
 *
 * @see InitCommand for initialization commands
 */
object Command {
	val setLogic = "set-logic"
	// TODO define all the other constants
}


trait Command extends Statement


sealed case class CommandAssert(val term: Term) extends Command {
    override def toString(): String = {
        return "(assert " + term + ")"
    }
}

sealed case class CommandDeclareSort(val symbol: Symbol, val value: AnyVal) 
		extends Command {
    override def toString(): String = {
        return new StringBuilder("(declare-sort ")
        	.append(symbol)
        	.append(" ")
        	.append(value)
        	.append(")")
        	.toString()
    }
}

sealed case class CommandDefineSort(
        val symbol: Symbol, val body: List[Symbol], val sort: Sort) 
		extends Command {
    override def toString(): String = {
        return new StringBuilder("(define-sort ")
        	.append(symbol)
        	.append(" (")
        	.append(body.mkString(" "))
        	.append(") ")
        	.append(sort)
        	.append(")")
        	.toString()
    }
}

sealed case class CommandDeclareFun(val name: Symbol, 
        val args: List[Sort], val returnType: Sort) 
		extends Command {
    override def toString(): String = {
        return new StringBuilder("(declare-fun ")
        	.append(name)
        	.append(" (")
        	.append(args.mkString(" "))
        	.append(") ")
        	.append(returnType)
        	.append(")")
        	.toString()
    }
}

sealed case class CommandDefineFun(val name: Symbol,
        val body: List[SortedVar],
        val sort: Sort, val term: Term) 
		extends Command {
    override def toString(): String = {
        return new StringBuilder("(define-fun ")
        	.append(name)
        	.append(" (")
        	.append(body.mkString(" "))
        	.append(") ")
        	.append(sort)
        	.append(" ")
        	.append(term)
        	.append(")")
        	.toString()
    }
}

sealed case class CommandCheckSat() extends Command {
    override def toString(): String = {
        return "(check-sat)"
    }
}

sealed case class CommandGetModel() extends Command {
    override def toString(): String = {
        return "(get-model)"
    }
}

sealed case class CommandExit() extends Command {
    override def toString(): String = {
        return "(exit)"
    }
}

sealed case class CommandPush() extends Command {
    override def toString(): String = {
        return "(push)"
    }
}

sealed case class CommandPop() extends Command {
    override def toString(): String = {
        return "(pop)"
    }
}

sealed case class CommandEcho(val msg: String) extends Command {
    override def toString(): String = {
        return "(echo " + msg + ")"
    }
}

