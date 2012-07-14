/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.Z3

import org.junit._
import Assert._
import z3.scala._
import it.polimi.nuzot.smt.SMTParser
import it.polimi.nuzot.smt.grammar._


/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 */
class Z3InterpreterTestCase {

    @Test
    def testVisitScript(): Unit = {
        val foo = "(declare-fun x () Int)\n" +
        		"(declare-fun y () Int)\n" +
        		"(declare-fun z () Int)\n" +
        		"(assert (>= (* 2 x) (+ y z)))\n" +
        		"(declare-fun f (Int) Int)\n" +
        		"(declare-fun g (Int Int) Int)\n" +
        		"(assert (< (f x) (g x x)))\n" +
        		"(assert (> (f y) (g x x)))\n" +
        		"(check-sat)\n" +
        		"(get-model)\n" +
        		"(push)\n" +
        		"(assert (= x y))\n" +
        		"(check-sat)\n" +
        		"(pop)\n" +
        		"(exit)"
        		
        val interpreter = new SMTParser()
        val script = interpreter.parseAll(interpreter.script, foo)
        println(script.getOrElse(null))
        
	    val z3 = new Z3Interpreter()
        z3.doVisit(script.getOrElse(null)); 
    }
    
    @Test
    def testVistKnownFunctions_Add(): Unit = {
        val interpreter = new SMTParser()
        val t1 =  interpreter.parseAll(interpreter.term, "5").getOrElse(null)
        val t2 = interpreter.parseAll(interpreter.term, "10").getOrElse(null)
        val add = Add(t1, t2)
        val z3Interpreter = new Z3Interpreter()
        val cfg = new Z3Config("MODEL" -> true)
	    val z3 = new Z3Context(cfg)
        val results = z3Interpreter.visitKnownFunctions(add)
    }
    
    @Test
    def testVisitTrueFalseAssertion(): Unit = {
        val foo = "(declare-const x Bool)\n" +
        		"(push)\n" +
        		"(assert (= x true))\n" +
        		"(check-sat)\n" +
        		"(get-model)\n" +
        		"(push)\n" +
        		"(assert (= x false))\n" +
        		"(check-sat)\n" +
        		"(get-model)\n" +
        		"(exit)"
        		
        val interpreter = new SMTParser()
        val script = interpreter.parseAll(interpreter.script, foo)
        println(script.getOrElse(null))
        
	    val z3 = new Z3Interpreter()
        z3.doVisit(script.getOrElse(null)); 
    }

}