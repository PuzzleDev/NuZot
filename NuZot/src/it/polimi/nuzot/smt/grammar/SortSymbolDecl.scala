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
 * <sort_symbol_decl> ::= ( <identifier> <numeral> <attribute>* )
 */
case class SortSymbolDecl(
        val identifier: Identifier, 
        val value: AnyVal, 
        val attributes: List[Attribute]) extends Statement {
    def this(identifier: Identifier, 
    		value: AnyVal) = {
        this(identifier, value, null)
    }
    
    override def toString(): String = {
        val builder = new StringBuilder("( ")
        	.append(identifier)
        	.append(" ")
        	.append(value)
        if (attributes != null && attributes != List()) {
            builder.append(" ")
        		.append(attributes.mkString(" "))
        }
        return builder.append(" )")
        	.toString()
    }
}