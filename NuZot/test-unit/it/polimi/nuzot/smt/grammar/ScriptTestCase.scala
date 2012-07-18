/**
 *
 */
package it.polimi.nuzot.smt.grammar

import org.junit._
import Assert._

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 */
class ScriptTestCase {

    @Test
    def testPrevious(): Unit = {
        val com1 = CommandEcho("com1")
        val com2 = CommandEcho("com2")
        val script = new Script() :+ com1 :+ com2 
        assertEquals(Script(List(com1)), script.previous())
    }
}