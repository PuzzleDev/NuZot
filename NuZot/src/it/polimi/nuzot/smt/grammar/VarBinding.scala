/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.smt.grammar

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 * <var_binding> ::= ( <symbol> <term> )
 */
case class VarBinding(val symbol: Symbol, val term: Term) extends Statement {

    override def toString(): String = {
        return "( " + symbol + " " + term + " )"
    }
}