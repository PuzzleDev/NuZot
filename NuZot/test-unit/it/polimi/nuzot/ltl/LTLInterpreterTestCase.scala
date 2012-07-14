/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.ltl

import org.junit._
import Assert._
import it.polimi.nuzot.shell.ShellInterpreter
import it.polimi.nuzot.smt.grammar._
import it.polimi.nuzot.Z3.Z3Interpreter
import it.polimi.nuzot.shell.ShellParser
import it.polimi.nuzot.smt.TypeChecker


/**
 * @author Michele Sama (m.sama@puzzledev.com)
 * 
 * TestCase for the LTLInterpreter
 */
object LTLInterpreterTestCase {
    val k: Int = 5
}

class LTLInterpreterTestCase {

    val interpreter = new LTLInterpreter()
    
    def preconditions(interpreter: LTLInterpreter): String =
        	"(declare-fun iLoop () " + interpreter.domain + ")\n" +
        	"(declare-fun loopex () Bool)\n" +
			"(assert (or (! loopex) (and (< " + interpreter.const(0) + " iLoop) (<= iLoop " + 
				interpreter.const(LTLInterpreterTestCase.k) +"))))"
        	
    def supportFz(name: String, time: Int): String = {
        return "(declare-fun " + name + " (Int) Bool)\n" +
     			"(assert (" + name + " " + time + "))\n" +
     			"(assert (-> " + LTLInterpreter.loopExLabel + " (iff (" + 
     					name + " " + (LTLInterpreterTestCase.k + 1) + ") (" + 
     					name + " " + LTLInterpreter.iLoopLabel + "))))\n" +
     			"(assert (-> (! " + LTLInterpreter.loopExLabel + ") (! (" + 
     					name + " " + (LTLInterpreterTestCase.k + 1) + "))))"
    }
    
    @Before
    def setUp = {
        //interpreter.temporalExt = LTLInterpreterTestCase.k
    }
    
    @Test
    def testDeneg_NotNot() = {
        assertEquals(Term.True, interpreter.deneg(Not(Not(Term.True))))
    }
    
    @Test
    def testDeneg_NotAnd() = {
        assertEquals(Or(Term.False, Term.False),
                interpreter.deneg(Not(And(Term.True, Term.True)))
                )
    }
    
    @Test
    def testDeneg_NotOr() = {
        assertEquals(And(Term.False, Term.False),
                interpreter.deneg(Not(Or(Term.True, Term.True)))
                )
    }
    
    
    /**
     * Assert the correct expansion of LTL function
     * in a working environment.
     * 
     * <p>This could be considered an integration test
     * because it not only checks the current expansion
     * of a single operator but it also checks its
     * evaluation (e.g. its satisfiability)
     */
    def assertExecution(input: String, expected: String, result: Boolean) = {
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

        assertEquals(
                "The shell interpreter has received a wrong input",
                input, shell.original.toString())
        assertEquals(
                "The shell interpreter should not have changed an LTL command",
                input, ltl.original.toString())
        assertEquals(
                "The temporal expansion is different than the one expected",
                expected, z3.original.toString())
        assertEquals(
                "The satisfiability is different than the one expected",
                result, z3.checkSat())
    }
    
    @Test
    def testTemporalAssert() = {
        val input = 
            	"(set-info :k 5)\n" +
            	"(declare-tfun x () Bool)\n" +
        		"(assert-t x 2)\n" +
        		"(assert-t (! x) 4)" 

        val expected = 
            	"(set-info :k 5)\n" +
            	preconditions(interpreter) + "\n" +
				"(declare-fun x (Int) Bool)\n" +
				"(assert (x 2))\n" +
				"(assert (and loopex (iff (x (- iLoop 1)) (x 5))))\n" +
				supportFz("zot-p0", 4) + "\n" +
				"(assert (iff (zot-p0 0) (! (x 0))))\n" +
				"(assert (iff (zot-p0 1) (! (x 1))))\n" +
				"(assert (iff (zot-p0 2) (! (x 2))))\n" +
				"(assert (iff (zot-p0 3) (! (x 3))))\n" +
				"(assert (iff (zot-p0 4) (! (x 4))))\n" +
				"(assert (iff (zot-p0 5) (! (x 5))))\n" +
				"(assert (iff (zot-p0 6) (! (x 6))))\n" +
				"(assert (and loopex (iff (x (- iLoop 1)) (x 5))))"
        
        assertExecution(input, expected, true)
    }
    
    @Test
    def testVisitPreconditionsInt() = {
        val k = AttributeKeyVal(Keyword(":k"),
        		AttributeValueSpecConst(SpecIntConstant(5)))
        
        interpreter.attributes = Map(k.keyword -> k)
         
        interpreter.domain = Sort.Int
        assertEquals(preconditions(interpreter),
                interpreter.generatePreconditions().toString())
    }
    
    @Test
    def testVisitPreconditionsReal() = {
        val k = AttributeKeyVal(Keyword(":k"),
        		AttributeValueSpecConst(SpecDoubleConstant(5)))
        
        interpreter.attributes = Map(k.keyword -> k)
        
        interpreter.domain = Sort.Real
        assertEquals(preconditions(interpreter),
                interpreter.generatePreconditions().toString())
    }
    
    @Test
    def testDeclareTFunInt() = {
        var input = 
            "(set-info :domain Int)\n" +
            "(set-info :k 5)\n" +
        	"(declare-tfun x () Bool)"
        var expected = 
            "(set-info :domain Int)\n" +
            "(set-info :k 5)\n" +
            preconditions(interpreter) + "\n" +
     		"(declare-fun x (Int) Bool)"
        
        assertExecution(input, expected, true)
    }
    
    @Test
    def testDeclareTFunReal() = {
        val input = 
            "(set-info :domain Real)\n" +
            "(set-info :k 5.0)\n" +
        	"(declare-tfun x () Bool)"
            
        interpreter.domain = Sort.Real  
        val expected = 
            "(set-info :domain Real)\n" +
            "(set-info :k 5.0)\n" +
            preconditions(interpreter) + "\n" +
     		"(declare-fun x (Real) Bool)"
        
        assertExecution(input, expected, true)
    }
    
    @Test
    def testNext() = {
        val input = 
            	"(set-info :k 5)\n" +
            	"(declare-tfun x () Bool)\n" +
        		"(assert (next x))"

        val expected =
            	"(set-info :k 5)\n" +
            	preconditions(interpreter) + "\n" +
     			"(declare-fun x (Int) Bool)\n" +
     			supportFz("zot-p0", 1) + "\n" +
     			"(assert (iff (zot-p0 0) (x 1)))\n" +
     			"(assert (iff (zot-p0 1) (x 2)))\n" +
     			"(assert (iff (zot-p0 2) (x 3)))\n" +
     			"(assert (iff (zot-p0 3) (x 4)))\n" +
     			"(assert (iff (zot-p0 4) (x 5)))\n" +
     			"(assert (iff (zot-p0 5) (x 6)))\n" +
     			"(assert (and loopex (iff (x (- iLoop 1)) (x 5))))"
     			
     	 assertExecution(input, expected, true)
    }
    
    @Test
    def testYesterday() = {
        val input = 
            	"(set-info :k 5)\n" +
            	"(declare-tfun x () Bool)\n" +
        		"(assert (yesterday x))"

        val expected = 
            	"(set-info :k 5)\n" +
            	preconditions(interpreter) + "\n" +
     			"(declare-fun x (Int) Bool)\n" +
     			supportFz("zot-p0", 1) + "\n" +
     			"(assert (iff (zot-p0 1) (x 0)))\n" +
     			"(assert (iff (zot-p0 2) (x 1)))\n" +
     			"(assert (iff (zot-p0 3) (x 2)))\n" +
     			"(assert (iff (zot-p0 4) (x 3)))\n" +
     			"(assert (iff (zot-p0 5) (x 4)))\n" +
     			"(assert (iff (zot-p0 6) (x 5)))\n" +
     			"(assert (and loopex (iff (x (- iLoop 1)) (x 5))))"
     			
     	assertExecution(input, expected, true)
    }
    
    @Test
    def testNot() = {
        val input =
            	"(set-info :k 5)\n" +
            	"(declare-tfun x () Bool)\n" +
        		"(assert (! x))"
        
        val expected = 
            	"(set-info :k 5)\n" +
            	preconditions(interpreter) + "\n" +
     			"(declare-fun x (Int) Bool)\n" +
     			supportFz("zot-p0", 1) + "\n" +
     			"(assert (iff (zot-p0 0) (! (x 0))))\n" +
     			"(assert (iff (zot-p0 1) (! (x 1))))\n" +
     			"(assert (iff (zot-p0 2) (! (x 2))))\n" +
     			"(assert (iff (zot-p0 3) (! (x 3))))\n" +
     			"(assert (iff (zot-p0 4) (! (x 4))))\n" +
     			"(assert (iff (zot-p0 5) (! (x 5))))\n" +
     			"(assert (iff (zot-p0 6) (! (x 6))))\n" +
     			"(assert (and loopex (iff (x (- iLoop 1)) (x 5))))"
     	
     	assertExecution(input, expected, true)
    }
    
    @Test
    def testAnd() = {
        val input = 
            	"(set-info :k 5)\n" +
            	"(declare-tfun x () Bool)\n" +
        		"(declare-tfun y () Bool)\n" +
        		"(assert (and x y))"
        
        val expected =
            	"(set-info :k 5)\n" +
            	preconditions(interpreter) + "\n" +
     			"(declare-fun x (Int) Bool)\n" +
     			"(declare-fun y (Int) Bool)\n" +
     			supportFz("zot-p0", 1) + "\n" +
     			"(assert (iff (zot-p0 0) (and (x 0) (y 0))))\n" +
     			"(assert (iff (zot-p0 1) (and (x 1) (y 1))))\n" +
     			"(assert (iff (zot-p0 2) (and (x 2) (y 2))))\n" +
     			"(assert (iff (zot-p0 3) (and (x 3) (y 3))))\n" +
     			"(assert (iff (zot-p0 4) (and (x 4) (y 4))))\n" +
     			"(assert (iff (zot-p0 5) (and (x 5) (y 5))))\n" +
     			"(assert (iff (zot-p0 6) (and (x 6) (y 6))))\n" +
     			"(assert (and loopex (iff (x (- iLoop 1)) (x 5))))\n" +
     			"(assert (and loopex (iff (y (- iLoop 1)) (y 5))))"
     	
     	assertExecution(input, expected, true)
    }
    
    @Test
    def testOr() = {
        val input = 
            	"(set-info :k 5)\n" +
            	"(declare-tfun x () Bool)\n" +
        		"(declare-tfun y () Bool)\n" +
        		"(assert (or x y))"
        
        val expected = 
            	"(set-info :k 5)\n" +
            	preconditions(interpreter) + "\n" +
     			"(declare-fun x (Int) Bool)\n" +
     			"(declare-fun y (Int) Bool)\n" +
     			supportFz("zot-p0", 1) + "\n" +
     			"(assert (iff (zot-p0 0) (or (x 0) (y 0))))\n" +
     			"(assert (iff (zot-p0 1) (or (x 1) (y 1))))\n" +
     			"(assert (iff (zot-p0 2) (or (x 2) (y 2))))\n" +
     			"(assert (iff (zot-p0 3) (or (x 3) (y 3))))\n" +
     			"(assert (iff (zot-p0 4) (or (x 4) (y 4))))\n" +
     			"(assert (iff (zot-p0 5) (or (x 5) (y 5))))\n" +
     			"(assert (iff (zot-p0 6) (or (x 6) (y 6))))\n" +
     			"(assert (and loopex (iff (x (- iLoop 1)) (x 5))))\n" +
     			"(assert (and loopex (iff (y (- iLoop 1)) (y 5))))"
     			
     	 assertExecution(input, expected, true)
    }
    
    @Test
    def testUntil() = {
        val input =
            	"(set-info :k 5)\n" +
            	"(declare-tfun x () Bool)\n" +
        		"(declare-tfun y () Bool)\n" +
        		"(assert (until x y))"
        
        val expected = 
            	"(set-info :k 5)\n" +
            	preconditions(interpreter) + "\n" +
     			"(declare-fun x (Int) Bool)\n" +
     			"(declare-fun y (Int) Bool)\n" +
     			supportFz("zot-p0", 1) + "\n" +
     			"(assert (iff (zot-p0 0) (or (y 0) (and (x 0) (zot-p0 1)))))\n" +
     			"(assert (iff (zot-p0 1) (or (y 1) (and (x 1) (zot-p0 2)))))\n" +
     			"(assert (iff (zot-p0 2) (or (y 2) (and (x 2) (zot-p0 3)))))\n" +
     			"(assert (iff (zot-p0 3) (or (y 3) (and (x 3) (zot-p0 4)))))\n" +
     			"(assert (iff (zot-p0 4) (or (y 4) (and (x 4) (zot-p0 5)))))\n" +
     			"(assert (iff (zot-p0 5) (or (y 5) (and (x 5) (zot-p0 6)))))\n" +
     			"(declare-fun i_eve_zot-p0 () Int)\n" +
     			"(assert (-> loopex (-> (zot-p0 5) (and (<= iLoop i_eve_zot-p0) " +
     				"(<= i_eve_zot-p0 5) (zot-p0 i_eve_zot-p0)))))\n" +
     			"(assert (and loopex (iff (x (- iLoop 1)) (x 5))))\n" +
     			"(assert (and loopex (iff (y (- iLoop 1)) (y 5))))"
     			
     	
     	assertExecution(input, expected, true)
    }
    
    @Test
    def testRelease() = {
        val input =
            	"(set-info :k 5)\n" +
            	"(declare-tfun x () Bool)\n" +
        		"(declare-tfun y () Bool)\n" +
        		"(assert (release x y))"
        
        val expected = 
            	"(set-info :k 5)\n" +
            	preconditions(interpreter) + "\n" +
     			"(declare-fun x (Int) Bool)\n" +
     			"(declare-fun y (Int) Bool)\n" +
     			supportFz("zot-p0", 1) + "\n" +
     			"(assert (iff (zot-p0 0) (and (y 0) (or (x 0) (zot-p0 1)))))\n" +
     			"(assert (iff (zot-p0 1) (and (y 1) (or (x 1) (zot-p0 2)))))\n" +
     			"(assert (iff (zot-p0 2) (and (y 2) (or (x 2) (zot-p0 3)))))\n" +
     			"(assert (iff (zot-p0 3) (and (y 3) (or (x 3) (zot-p0 4)))))\n" +
     			"(assert (iff (zot-p0 4) (and (y 4) (or (x 4) (zot-p0 5)))))\n" +
     			"(assert (iff (zot-p0 5) (and (y 5) (or (x 5) (zot-p0 6)))))\n" +
     			"(declare-fun i_eve_zot-p0 () Int)\n" +
     			"(assert (-> loopex (-> (! (zot-p0 5)) (and (<= iLoop i_eve_zot-p0) " +
     			"(<= i_eve_zot-p0 5) (! (zot-p0 i_eve_zot-p0))))))\n" +
     			"(assert (and loopex (iff (x (- iLoop 1)) (x 5))))\n" +
     			"(assert (and loopex (iff (y (- iLoop 1)) (y 5))))"
     			
     	assertExecution(input, expected, true)
    }
    
    @Test
    def testSince() = {
        val input =
            	"(set-info :k 5)\n" +
            	"(declare-tfun x () Bool)\n" +
        		"(declare-tfun y () Bool)\n" +
        		"(assert (since x y))"

        val expected = 
            	"(set-info :k 5)\n" +
            	preconditions(interpreter) + "\n" +
     			"(declare-fun x (Int) Bool)\n" +
     			"(declare-fun y (Int) Bool)\n" +
     			supportFz("zot-p0", 1) + "\n" +
     			"(assert (iff (zot-p0 0) (y 0)))\n" +
     			"(assert (iff (zot-p0 1) (or (y 1) (and (x 1) (zot-p0 0)))))\n" +
				"(assert (iff (zot-p0 2) (or (y 2) (and (x 2) (zot-p0 1)))))\n" +
				"(assert (iff (zot-p0 3) (or (y 3) (and (x 3) (zot-p0 2)))))\n" +
				"(assert (iff (zot-p0 4) (or (y 4) (and (x 4) (zot-p0 3)))))\n" +
				"(assert (iff (zot-p0 5) (or (y 5) (and (x 5) (zot-p0 4)))))\n" +
				"(assert (iff (zot-p0 6) (or (y 6) (and (x 6) (zot-p0 5)))))\n" +
				"(assert (and loopex (iff (x (- iLoop 1)) (x 5))))\n" +
     			"(assert (and loopex (iff (y (- iLoop 1)) (y 5))))"
		
     	assertExecution(input, expected, true)
    }
    
    @Test
    def testTrigger() = {
        val input =
            	"(set-info :k 5)\n" +
            	"(declare-tfun x () Bool)\n" +
        		"(declare-tfun y () Bool)\n" +
        		"(assert (trigger x y))"
        
        val expected = 
            	"(set-info :k 5)\n" +
            	preconditions(interpreter) + "\n" +
     			"(declare-fun x (Int) Bool)\n" +
     			"(declare-fun y (Int) Bool)\n" +
     			supportFz("zot-p0", 1) + "\n" +
     			"(assert (iff (zot-p0 0) (y 0)))\n" +
     			"(assert (iff (zot-p0 1) (and (y 1) (or (x 1) (zot-p0 0)))))\n" +
				"(assert (iff (zot-p0 2) (and (y 2) (or (x 2) (zot-p0 1)))))\n" +
				"(assert (iff (zot-p0 3) (and (y 3) (or (x 3) (zot-p0 2)))))\n" +
				"(assert (iff (zot-p0 4) (and (y 4) (or (x 4) (zot-p0 3)))))\n" +
				"(assert (iff (zot-p0 5) (and (y 5) (or (x 5) (zot-p0 4)))))\n" +
				"(assert (iff (zot-p0 6) (and (y 6) (or (x 6) (zot-p0 5)))))\n" +
				"(assert (and loopex (iff (x (- iLoop 1)) (x 5))))\n" +
     			"(assert (and loopex (iff (y (- iLoop 1)) (y 5))))"

     	assertExecution(input, expected, true)
    }
    
    @Test
    def testZeta() = {
        val input = 
            	"(set-info :k 5)\n" +
            	"(declare-tfun x () Bool)\n" +
        		"(assert (zeta x))"

        val expected = 
            	"(set-info :k 5)\n" +
            	preconditions(interpreter) + "\n" +
     			"(declare-fun x (Int) Bool)\n" +
     			supportFz("zot-p0", 1) + "\n" +
     			"(assert (zot-p0 0))\n" +
     			"(assert (iff (zot-p0 1) (x 0)))\n" +
     			"(assert (iff (zot-p0 2) (x 1)))\n" +
     			"(assert (iff (zot-p0 3) (x 2)))\n" +
     			"(assert (iff (zot-p0 4) (x 3)))\n" +
     			"(assert (iff (zot-p0 5) (x 4)))\n" +
     			"(assert (iff (zot-p0 6) (x 5)))\n" +
     			"(assert (and loopex (iff (x (- iLoop 1)) (x 5))))"
     	
     	assertExecution(input, expected, true)
    }
}