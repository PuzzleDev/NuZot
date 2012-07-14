/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.smt.grammar

import z3.scala._

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 * 
 * <attribute> ::= <keyword> | <keyword> <attribute_value>
 */
abstract class Attribute(val keyword: Keyword) extends Statement


sealed case class AttributeKey(override val keyword: Keyword)
		extends Attribute(keyword) {
    
	override def toString(): String = {
		return keyword.toString()
	}
}


sealed case class AttributeKeyVal(
        override val keyword: Keyword, val attributeValue: AttributeValue)
		extends Attribute(keyword) {
	
    override def toString(): String = {
		return keyword.toString() + " " + attributeValue.toString()
	}
}