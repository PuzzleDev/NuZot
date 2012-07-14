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
 * TestCase for class SExpr
 */
class SExprTestCase {
	@Test
	def testCreateSpecConstant(): Unit = {
	    val sExpr: SExpr = SExprSpecConstant(SpecStringConstant("prova"))
	}
	
	@Test
	def testCreateSymbol(): Unit = {
	    val sExpr: SExpr = SExprSymbol(Symbol("prova"))
	}
	
	@Test
	def testCreateKeyword(): Unit = {
	    val sExpr: SExpr = SExprKeyword(Keyword(":prova"))
	}
	
	@Test
	def testCreateEmpty(): Unit = {
	    val sExpr: SExpr = SExprMulti()
	}
	
	@Test
	def testCreateQueue(): Unit = {
	    val sExpr: SExpr = SExprMulti(
		        SExprSymbol(Symbol("prova")), 
		        SExprSymbol(Symbol("prova"))
		        )
	}
}