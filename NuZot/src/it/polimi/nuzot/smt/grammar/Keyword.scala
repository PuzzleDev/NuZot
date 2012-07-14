/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.smt.grammar

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 * <keyword> ::== :[a-zA-Z0-9~!@$%^&\*_\-\+=<>\.\?/]+
 */
object Keyword {
    val pattern = """(:[a-zA-Z0-9~!@$%^&\*_\-\+=<>\.\?/]+)""".r
}

sealed case class Keyword(val name: String) extends Statement {
	private def checkParam(string: String) {
		string match {
			case Keyword.pattern(string) => {}
			case _ => {
				throw new IllegalArgumentException(
					  "Illegal keyword name: " + string);
			}
		}
	}
	checkParam(name)
	
	override def toString(): String = {
	    return name
	}
}