/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.ltl

import org.junit._
import Assert._

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 * Test case for the LTL parser
 */
class LTLParserTestCase {

    val parser = new LTLParser()
    
    @Test
    def testParseAssertT = {
        val command = "(assert-t (not x) 4)"
        var res = parser.parseAll(parser.command, command)
        assertEquals(command, res.getOrElse().toString())
    }
    
    @Test
    def testParseDeclareFun_noArgs = {
        val command = "(declare-tfun x Int)"
        val expected = "(declare-tfun x () Int)"
        var res = parser.parseAll(parser.command, command)
        assertEquals(expected, res.getOrElse().toString())
    }
}