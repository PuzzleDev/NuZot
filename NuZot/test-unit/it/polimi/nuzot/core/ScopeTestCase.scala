/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.core

import org.junit._
import Assert._

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 */
class ScopeTestCase {

    @Test
    def testPushPop(): Unit = {
        val asd = "ads"
        var scope = new Scope[String, String]()
        scope.set(asd, asd)
        assertEquals(asd, scope.get(asd).getOrElse())
        scope = scope.push()
        assertEquals(asd, scope.get(asd).getOrElse())
        scope = scope.pop().getOrElse(null)
        assertEquals(asd, scope.get(asd).getOrElse())
    }
}