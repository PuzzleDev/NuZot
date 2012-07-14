/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.core

import org.junit._
import Assert._
import it.polimi.nuzot.ltl.LTLInterpreter
import it.polimi.nuzot.shell.ShellParser
import it.polimi.nuzot.shell.ShellInterpreter
import it.polimi.nuzot.smt.TypeChecker
import it.polimi.nuzot.smt.grammar.Script
import it.polimi.nuzot.Z3.Z3Interpreter
import z3.scala.Z3Model


/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 */
class NuZotIntegrationTestCase {

    /**
     * Assert the correct execution of a script
     * in a working environment.
     */
    def assertExecution(input: String, expected: Boolean): Z3Model = {
        val parser = new ShellParser()
        val inputScript = parser.parseAll(
		                parser.script, input)
		                .getOrElse(null)
		
		// Recreates a complete execution stack
		val z3 = new Z3Interpreter()
        val typeChecker = new TypeChecker()
        typeChecker.next(z3)
		val ltl = new LTLInterpreter()
    	ltl.next(typeChecker)
		val shell = new ShellInterpreter()
        shell.next(ltl)
    			
        shell.doVisit(inputScript)
        assertEquals(input, shell.original.toString())
        assertEquals(input, ltl.original.toString())
        assertEquals(expected, z3.checkSat())
        
        z3.z3.checkAndGetModel match {
	        case (None, _) => {
	            fail(z3.z3.getSearchFailure.message)
	            return null;
	        }
	        case (Some(false), _) => {
	            return null;
	        }
	        case (Some(true), model) => {
	            return model
	        }
        }
    }
    
    @Test
    def testModelForNext(): Unit = {
        val input = 
            	"(set-info :k 5)\n" +
            	"(declare-tfun x () Bool)\n" +
        		"(assert (next x))"
        val model = assertExecution(input, true)
        println("---------")
        model.getModelConstantInterpretations.foreach(x => {
            println(x._1.getName + ": " + x._2)
        })
        println("---------")
        model.getModelFuncInterpretations.foreach(x => {
            println(x._1.getName + ": " + x._2)
        })
    }
    
    @Test
    def testDoubleNext(): Unit = {
        val input = 
            	"(set-info :k 5)\n" +
            	"(declare-tfun x () Bool)\n" +
        		"(assert (next (next x)))"
        assertExecution(input, true)
    }
    
    @Test
    def testNextUntil(): Unit = {
        val input = 
            	"(set-info :k 5)\n" +
            	"(declare-tfun x () Bool)\n" +
            	"(declare-tfun y () Bool)\n" +
        		"(assert (next (until x y)))"
        assertExecution(input, true)
    }
    
    @Test
    def testUntilGt(): Unit = {
        val input = 
            	"(set-info :k 5)\n" +
            	"(declare-tfun x () Int)\n" +
            	"(declare-tfun y () Int)\n" +
            	"(declare-tfun z () Bool)\n" +
        		"(assert (until (> x y) z))"
        assertExecution(input, true)
    }
    
        
    @Test
    def testRealEquality(): Unit = {
        val input = 
            "(set-info :domain Real)\n" +
            "(set-info :k 5.0)\n" +
            "(assert (= 0.1 0.7))"
        assertExecution(input, false)
    }
    
    @Test
    def testImmediateExit(): Unit = {
        val input = "(exit)"
        // Checking solution after exiting 
        // should throw an exception
        try {
            assertExecution(input, false)
            fail("Checking solution after exiting " +
            		"should throw an exception")
        } catch {
            case ex: IllegalStateException => {
                // ok
            }
        }
        
    }
}