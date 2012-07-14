/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.smt.grammar

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 * 
 * Defines a literal in the SMT grammar
 */
object Symbol {
    val pattern = """([a-zA-Z~!@$%^&\*_\-\+=<>\.\?/][a-zA-Z0-9~!@\$%^&\*_\-\+=<>\.\?/]*)""".r
    
    val Bool = Symbol("Bool")
    val Int = Symbol("Int")
    val Real = Symbol("Real")
}

/**
 * <symbol> ::== [a-zA-Z~!@$%^&\*_\-\+=<>\.\?/][a-zA-Z0-9~!@\$%^&\*_\-\+=<>\.\?/]*
 */
case class Symbol(val name: String) extends Statement {
	private def checkParam(string: String) {
		string match {
			case Symbol.pattern(string) => {}
			case _ => {
				throw new IllegalArgumentException(
					  "Illegal symbol name: " + string);
			}
		}
	}
	checkParam(name)
	
	override def toString(): String = return name
}