package it.polimi.nuzot.smt.grammar

import org.junit._
import Assert._

class AttributeTestCase {
    
	@Test
	def testCreateKeyword(): Unit = {
	    AttributeKey(Keyword(":foo"))
	    try {
	        AttributeKey(Keyword("noSemicolonKeyword"))
	        fail("Empty keyword should have failed.")
	    } catch {
			case ex: Exception => {}
		}
	    try {
	        AttributeKey(Keyword(""))
	        fail("Empty keyword should have failed.")
	    } catch {
			case ex: Exception => {}
		}
	}
	
	@Test
	def testCreateKeywordAttribute(): Unit = {
	    AttributeKeyVal(Keyword(":foo"),
	    		AttributeValueSymbol(Symbol("bar"))
	    		)
	}
	
	@Test
	def testKeywordToString(): Unit = {
	    val foo = ":foo"
	    val attr = AttributeKey(Keyword(foo))
	    assertEquals(foo, attr.toString())
	}
	
	@Test
	def testAttributeToString(): Unit = {
	    val foo = ":foo"
	    val bar = "bar"
	    val attr = AttributeKeyVal(Keyword(foo),
	            AttributeValueSymbol(Symbol(bar))
	            )
	    assertEquals(foo + " " + bar, attr.toString())
	}
}