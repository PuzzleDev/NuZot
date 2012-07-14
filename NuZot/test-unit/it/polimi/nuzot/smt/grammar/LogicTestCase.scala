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
class LogicTestCase {

    @Test
    def testToString(): Unit = {
        val foo = "foo"
        val attrs = List(
                )
        val logic = Logic(Symbol(foo),
                LogicAttribute.language("foo"),
                LogicAttribute.values("bar")
                )
        assertEquals(
                "( logic foo :language foo :values bar )",
                logic.toString())
    }
}