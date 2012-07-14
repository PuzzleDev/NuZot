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
class AttributeValueTestCase {
    
	@Test
	def testCreateSpecConstant(): Unit = {
	    AttributeValueSpecConst(SpecStringConstant("prova"))
	}
	
	@Test
	def testCreateSymbol(): Unit = {
	    AttributeValueSymbol(Symbol("prova"))
	}
	
	@Test
	def testCreateEmpty(): Unit = {
	    AttributeValueSExpr()
	}
	
	@Test
	def testCreateList(): Unit = {
	    AttributeValueSExpr(
		        SExprSymbol(Symbol("prova0")), 
		        SExprSymbol(Symbol("prova1"))
	    )
	}
	
	@Test
	def testSpecConstantToString(): Unit = {
	    val foo = "foo"
	    val attr = new AttributeValueSpecConst(SpecStringConstant(foo))
	    assertEquals(foo, attr.toString())
	}
	
	@Test
	def testSymbolToString(): Unit = {
	    val foo = "foo"
	    val attr = new AttributeValueSymbol(Symbol(foo))
	    assertEquals(foo, attr.toString())
	}
	
	@Test
	def testEmptyToString(): Unit = {
	    val foo = "foo"
	    val bar = "bar"
	    val attr = AttributeValueSExpr(
                SExprSymbol(Symbol(foo)), 
		        SExprSymbol(Symbol(bar))
	    )
	    assertEquals("( " + foo + " " + bar + " )",
	            attr.toString())
	}
}