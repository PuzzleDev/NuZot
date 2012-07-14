/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.core

import it.polimi.nuzot.smt.grammar._
import it.polimi.nuzot.ltl.LTLInterpreter

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 */
trait DSLInterpreterDelegate {
	
    var _attributes: List[LogicAttribute] = List()
    def attributes(attributes: List[LogicAttribute]) = {
        _attributes = attributes
    }
    
    def generatePreconditions(ltl: LTLInterpreter): Script
}