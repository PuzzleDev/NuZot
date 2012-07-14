/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.smt.grammar

import scala.collection.immutable.Queue

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 * <logic> ::== ( logic <symbol> <logic_attribute>+ )
 */
case class Logic(val symbol: Symbol, 
        val logicAttributes: LogicAttribute*) {

    override def toString(): String = {
        new StringBuilder("( logic ")
        	.append(symbol)
        	.append(" ")
        	.append(logicAttributes.mkString(" "))
        	.append(" )")
        	.toString()
    }
}