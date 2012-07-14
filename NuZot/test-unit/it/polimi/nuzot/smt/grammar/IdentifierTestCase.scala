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
 */
class IdentifierTestCase {
    
	@Test
	def testToString(): Unit = {
	    val foo = "foo"
	    val identifier = IdentifierSymbol(Symbol(foo))
	    assertEquals(foo, identifier.toString())
	}
	
	@Test
	def testNumeralToString(): Unit = {
	    val foo = "foo"
	    val attrs = List(1, 2, 3)
	    val identifier = IdentifierSymbolNum(Symbol(foo), attrs)
	    assertEquals("( _foo 1 2 3 )",
	            identifier.toString())
	}
}