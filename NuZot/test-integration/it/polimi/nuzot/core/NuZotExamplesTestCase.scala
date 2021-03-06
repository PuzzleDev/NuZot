/**
 *
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
 * @author  Michele Sama (m.sama@puzzledev.com)
 *
 * Uses the examples to check the validity
 * of the computed model.
 */
class NuZotExamplesTestCase {

    def assertFileExecution(
            filename: String, expected: Boolean): Z3Model = {
        val parser = new ShellParser()
        val inputScript = parser.loadFile(filename)
        
		// Recreates a complete execution stack
		val z3 = new Z3Interpreter()
        val typeChecker = new TypeChecker()
        typeChecker.next(z3)
		val ltl = new LTLInterpreter()
    	ltl.next(typeChecker)
		val shell = new ShellInterpreter()
        shell.next(ltl)
    			
        shell.doVisit(inputScript)
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
    def testAlways(): Unit = {
        var model = assertFileExecution(
                "examples/ltl/Always.zot", true);
    }
    
    @Test
    def testAndReleaseImp(): Unit = {
        var model = assertFileExecution(
                "examples/ltl/AndReleaseImp.zot", true);
    }
    
    @Test
    def testContraddictAlways(): Unit = {
        var model = assertFileExecution(
                "examples/ltl/ContradictAlways.zot", false);
    }
    
    @Test
    def testGreaterThan(): Unit = {
        var model = assertFileExecution(
                "examples/ltl/GreaterThan.zot", true);
    }
    
    @Test
    def testFalseLoop(): Unit = {
        var model = assertFileExecution(
                "examples/ltl/FalseLoop.zot", true);
    }
    
    @Test
    def testNextEventually(): Unit = {
        var model = assertFileExecution(
                "examples/ltl/NextEventually.zot", true);
    }
    
    @Test
    def testUntilYesterday(): Unit = {
        var model = assertFileExecution(
                "examples/ltl/UntilYesterday.zot", false);
    }
    
    @Test
    def testReleaseYesterday(): Unit = {
        var model = assertFileExecution(
                "examples/ltl/ReleaseYesterday.zot", true);
    }
    
    @Test
    def testUntilRelease(): Unit = {
        var model = assertFileExecution(
                "examples/ltl/UntilRelease.zot", false);
    }
    
    @Test
    def testYesterday2(): Unit = {
        var model = assertFileExecution(
                "examples/ltl/Yesterday2.zot", false);
    }
    
    @Test
    def testYesterday2Next(): Unit = {
        var model = assertFileExecution(
                "examples/ltl/Yesterday2Next.zot", false);
    }
    
    @Test
    def testYesterdayAndYesterday(): Unit = {
        var model = assertFileExecution(
                "examples/ltl/YesterdayAndYesterday.zot", false);
    }
    
    @Test
    def testZetaYesterday(): Unit = {
        var model = assertFileExecution(
                "examples/ltl/ZetaYesterday.zot", false);
    }
}