/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.core

import org.junit._
import Assert._
import it.polimi.nuzot.smt.grammar._

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 * Test case for the DSLInterpreter class
 */
class DSLInterpreterTestCase {

    @Test
    def testSetParamValues(): Unit = {
        class MockInterpreter extends DSLInterpreter {
            def generatePreconditions(): Script = {
                new Script()
            }
        }
        
        val interpreter = new MockInterpreter()
        
        val bar = AttributeKey(Keyword(":bar"))
        val foo = AttributeKeyVal(Keyword(":foo"),
        		AttributeValueSpecConst(SpecIntConstant(5)))
        
        val script = Script(List()) :+
        		InitCommandSetInfo(bar) :+
        		InitCommandSetInfo(foo)
        
        interpreter.doVisit(script)
        		
        val expected = Map(bar.keyword -> bar, foo.keyword -> foo)
        assertEquals(2, interpreter.attributes.size)
        assertEquals(expected, interpreter.attributes)
    }
}