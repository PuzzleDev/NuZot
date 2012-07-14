/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.smt.grammar

import scala.collection.immutable.Queue
import scala.util.parsing.combinator.JavaTokenParsers
import z3.scala._


/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 * Represent a SMT.lib grammar token.
 * This is the base DSL (domain-specific-language) class.
 */    
trait Statement
