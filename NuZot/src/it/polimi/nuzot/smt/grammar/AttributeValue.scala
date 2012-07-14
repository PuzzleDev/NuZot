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
 * <attribute_value> ::= <spec_constant>| <symbol> | ( s_expr* )
 */
abstract class AttributeValue(val value: Any) extends Statement


case class AttributeValueSpecConst(override val value: SpecConstant) 
		extends AttributeValue {

    override def toString(): String = {
        return value.toString()
    }

}

case class AttributeValueSymbol(override val value: Symbol)
		extends AttributeValue {

    override def toString(): String = {
        return value.toString()
    }

}

case class AttributeValueSExpr(override val value: SExpr*) 
		extends AttributeValue {
    override def toString(): String = {
        return new StringBuilder("( ")
        	.append(value.mkString(" "))
        	.append(" )")
        	.toString()
        }
}
