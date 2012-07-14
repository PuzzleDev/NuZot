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
class SortTestCase {

    @Test
    def testNoEmpty(): Unit = {
        try{
            SortIdentifier(IdentifierSymbol(Symbol("prova")))
        } catch {
            case ex: Exception => {}
        }
    }
}