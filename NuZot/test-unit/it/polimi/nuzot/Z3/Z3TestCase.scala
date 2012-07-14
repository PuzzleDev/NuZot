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


/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 */
class Z3TestCase {

    @Test
    def testZ3Configured(): Unit = {
        
	    val totSecs = 12345
	 
	    val cfg = new Z3Config("MODEL" -> true) // required if you plan to query models of satisfiable constraints
	    val z3 = new Z3Context(cfg)
	 
	    // prepares the integer sort and three constants (the "unknowns")
	    val i = z3.mkIntSort
	    val h = z3.mkConst(z3.mkStringSymbol("h"), i)
	    val m = z3.mkConst(z3.mkStringSymbol("m"), i)
	    val s = z3.mkConst(z3.mkStringSymbol("s"), i)
	    // builds a constant integer value from the CL arg.
	    val t = z3.mkInt(totSecs, i)
	    // more integer constants
	    val z = z3.mkInt(0, i)
	    val sx = z3.mkInt(60, i)
	
	    // builds the constraint h*3600 + m * 60 + s == totSecs
	    val cs1 = z3.mkEq(
	        z3.mkAdd(
	            z3.mkMul(z3.mkInt(3600, i), h),
	            z3.mkMul(sx, m),
	            s),
	        t)
	
	    // more constraints
	    val cs2 = z3.mkAnd(z3.mkGE(h, z), z3.mkLT(h, z3.mkInt(24, i)))
	    val cs3 = z3.mkAnd(z3.mkGE(m, z), z3.mkLT(m, sx))
	    val cs4 = z3.mkAnd(z3.mkGE(s, z), z3.mkLT(s, sx))
	
	    // pushes the constraints to the Z3 context
	    z3.assertCnstr(z3.mkAnd(cs1, cs2, cs3, cs4))
	
	    // attempting to solve the constraints, and reading the result
	    z3.checkAndGetModel match {
	        case (None, _) => println("Z3 failed. The reason is: " +
	                z3.getSearchFailure.message)
	        case (Some(false), _) => println("Unsat.")
	        case (Some(true), model) => {
	            println("h: " + model.evalAs[Int](h))
	            println("m: " + model.evalAs[Int](m))
	            println("s: " + model.evalAs[Int](s))
	            model.delete
	        }
	    }
	
	    z3.delete
    }
    
    @Test
    def testParseScript = {
        val foo = "(declare-fun x () Int)\n" +
        		"(declare-fun y () Int)\n" +
        		"(declare-fun z () Int)\n" +
        		"(assert (>= (* 2 x) (+ y z)))\n" +
        		"(declare-fun f (Int) Int)\n" +
        		"(declare-fun g (Int Int) Int)\n" +
        		"(assert (< (f x) (g x x)))\n" +
        		"(assert (> (f y) (g x x)))\n" +
        		"(check-sat)\n" +
        		//"(get-model)\n" +
        		"(push)\n" +
        		"(assert (= x y))\n" +
        		"(check-sat)\n" +
        		"(pop)\n" +
        		"(exit)"
        val interpreter = new SMTParser()
        val res = interpreter.parseAll(interpreter.script, foo)
        
        println(res.getOrElse())
        
        val cfg = new Z3Config("MODEL" -> true)
	    val z3 = new Z3Context(cfg)
        
        val z3ast = z3.parseSMTLIB2String(res.getOrElse().toString())
        println("AST: " + z3.astToString(z3ast))
        z3.checkAndGetModel match {
	        case (None, _) => println("Z3 failed. The reason is: " +
	                z3.getSearchFailure.message)
	        case (Some(false), _) => println("Unsat.")
	        case (Some(true), model) => {
	            println("MODEL: " + model)
	            model.delete
	        }
        }
    }
 
}