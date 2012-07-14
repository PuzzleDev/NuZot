/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.smt.grammar

import org.junit._
import Assert._

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 * TestCase for class Symbol
 */
class SymbolTestCase {
  
    def assertValidName(name: String): Unit = {
        try {
        	Symbol(name)
		} catch {
			case ex: Exception => {
			    fail("Valid name should not have thrown an exception: " + name)
			}
		}
    }
    
    def assertIllegalName(name: String): Unit = {
        try {
        	Symbol(name)
        	fail("Illegal name should have thrown an exception: " + name)
		} catch {
			case ex: Exception => {}
		}
    }

	@Test
	def testNames():  Unit = {
	    val validNames = List[String](
	            "aab", "+a", "+aab", "+1", "a0", "a0a", "@1asd")
	    val illegalNames = List[String](
	            "0asd", "#+0", "112", "1A0", "a#")
	    validNames.foreach(assertValidName(_))
	    illegalNames.foreach(assertIllegalName(_))
	}
}