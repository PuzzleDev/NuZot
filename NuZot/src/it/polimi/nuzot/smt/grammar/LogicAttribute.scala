/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.smt.grammar

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 * <logic_attribute> ::== :theories ( <symbol>+ )
 *		| :language <string>
 *		| :extensions <string>
 *		| :values <string>
 *		| :notes <string>
 *		| <attribute>
 */
object LogicAttribute {
    // TODO implement this as Command
    def theories(symbols: List[Symbol]): LogicAttribute = {
        LogicAttribute(Keyword(":theories"), symbols)
    }
    def language(string: String): LogicAttribute = {
        LogicAttribute(Keyword(":language"), string)
    }
    def extensions(string: String): LogicAttribute = {
        LogicAttribute(Keyword(":extensions"), string)
    }
    def values(string: String): LogicAttribute = {
        LogicAttribute(Keyword(":values"), string)
    }
    def notes(string: String): LogicAttribute = {
        LogicAttribute(Keyword(":notes"), string)
    }
    def attribute(attribute: Attribute): LogicAttribute = {
        LogicAttribute(null, attribute)
    }
}

case class LogicAttribute(val keyword: Keyword, val value:Any)
		extends Statement {
	
    override def toString(): String = {
        (keyword, value) match {
            case (null, x) => x.toString()
            case (x, y) => x + " " + y
        }
    }
}