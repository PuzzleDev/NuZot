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
 * Define an interpreter delegate. Each interpreter 
 * can have as many delegates as required, but only one
 * of each type can be active at the same time.
 * 
 * <p>Delegates are instantiated by class name using
 * the default constructor. Subclasses must have a
 * public default constructor.
 */
trait DSLInterpreterDelegate[T <: DSLInterpreter] {
    
    def generatePreconditions(interpreter: T): Script
}