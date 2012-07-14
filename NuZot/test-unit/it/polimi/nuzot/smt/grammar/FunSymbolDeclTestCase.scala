/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.smt.grammar

import org.junit._
import Assert._
import scala.collection.immutable.Queue

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 */
class FunSymbolDeclTestCase {

    @Test
    def testSpecToString(): Unit = {
        val foo = "foo"
        val bar = ":bar"
        val decl = FunSymbolDeclSpecConst(
                SpecStringConstant(foo), 
                SortIdentifier(IdentifierSymbol(Symbol(foo))),
                List(AttributeKey(Keyword(bar))))
       assertEquals("(foo foo :bar)", decl.toString())
    }
    
    @Test
    def testMetaSpecToString(): Unit = {
        val foo = "foo"
        val bar = ":bar"
        val decl = FunSymbolDeclMetaSpecConst(
                MetaSpecStringConst(foo), 
                SortIdentifier(IdentifierSymbol(Symbol(foo))),
                List(AttributeKey(Keyword(bar))))
       assertEquals("(foo foo :bar)", decl.toString())
    }
    
    @Test
    def testIdentifierToString(): Unit = {
        val foo = "foo"
        val bar = ":bar"
        val decl = FunSymbolDeclIdentifier(
                IdentifierSymbol(Symbol(foo)), 
                List(SortIdentifier(IdentifierSymbol(Symbol(foo)))),
                List(AttributeKey(Keyword(bar))))
       assertEquals("(foo foo :bar)", decl.toString())
    }
}