/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.smt

import org.junit._
import Assert._
import it.polimi.nuzot.smt.grammar._

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 * Test case for the TypeChecker
 */
class TypeCheckerTestCase {
	
    var interpreter: TypeChecker = null
    
    @Before
    def setUp(): Unit = {
        interpreter = new TypeChecker()
    }
    
    def createDisequalities(x: Term, y: Term): List[EqualityOperator] = {
        return LE(x, y) :: LT(x, y) ::
        		GE(x, y):: GT(x, y) :: List()
    }
    
    def assertType(t: Term, expected: Sort) {
        assertEquals(expected, interpreter.visitTerm(t))
    }
    
    @Test
    def testEqualities() {
        val x = Term.const(2)
        val y = Term.const(7)
        (EQ(x, y) :: createDisequalities(x, y)).foreach(
                assertType(_, Sort.Bool))
    }
    
    @Test
    def testDisequalityWithOneBoolean() {
        (createDisequalities(Term.const(2), Term.const(true)) :::
         createDisequalities(Term.const(false), Term.const(4)) :::    
         createDisequalities(Term.const(false), Term.const(true))   
        ).foreach(op =>{
            try {
            	assertType(op, Sort.Bool)
            	fail("Wrong args for Equality " + op + 
            			" should have failed.");
		    } catch {
	            case ex: Exception => {
                // Ok
	            }
	        }
        })    
    }
    
    @Test
    def testDisequalityWithDiffrentDomains() {
        interpreter.domain = Sort.Real
        createDisequalities(Term.const(2), Term.const(2.0)).foreach(op =>{
            try {
            	assertType(op, Sort.Bool)
            	fail("Wrong args for Equality " + op + 
            			" should have failed.");
		    } catch {
	            case ex: Exception => {
	            	// Ok
	            }
	        }
        })    
    }
    
    @Test
    def testIntWithRealDomain() {
        interpreter.domain = Sort.Real
        createDisequalities(Term.const(1), Term.const(2)).foreach(op =>{
            try {
            	assertType(op, Sort.Bool)
            	fail("Wrong args for Equality " + op + 
            			" should have failed.");
		    } catch {
	            case ex: Exception => {
	            	// Ok
	            }
	        }
        })    
    }
    
    @Test
    def testEQWithSame() {
        assertType(EQ(Term.const(true), Term.const(false)),
                Sort.Bool)
        assertType(EQ(Term.const(2), Term.const(4)),
                Sort.Bool)
    }
    @Test
    def testEQWithDifferentSort() {     
        try {
        	assertType(EQ(Term.const(true), Term.const(4)),
                Sort.Bool)
            fail("Wrong args for Equality " + 
        			" should have failed.");
	    } catch {
            case ex: Exception => {
                // Ok
            }
        }
    }
    
    @Test
    def testArithmeticWithDifferentSortShouldFail() = {
        interpreter.domain = Sort.Real
        try {
        	assertType(Add(Term.const(2.0), Term.const(4)),
                Sort.Real)
            fail("Arithmetic operator with different args domain " + 
        			" should have failed.");
	    } catch {
            case ex: Exception => {
                // Ok
            }
        }
    }
    
    @Test
    def testArithmeticWithWrongDomainSortShouldFail() = {
        interpreter.domain = Sort.Real
        try {
        	assertType(Add(Term.const(2), Term.const(4)),
                Sort.Real)
            fail("Arithmetic operator with wrong args domain " + 
        			" should have failed.");
	    } catch {
            case ex: Exception => {
                // Ok
            }
        }
    }
    
    @Test
    def testArithmetic() = {
        interpreter.domain = Sort.Real
        assertType(Sub(Term.const(2.0), Term.const(0.4)),
                Sort.Real)
        
        interpreter.domain = Sort.Int
        assertType(Sub(Term.const(2), Term.const(4)),
                Sort.Int)
    }
    
    @Test
    def testBooleanFunction(): Unit = {
        val name = Symbol("s")
        interpreter.doVisit(
                Script(List(CommandDeclareFun(name, List(), Sort.Int))))
        assertEquals(Some(List()), interpreter.scopeFunctionArgs.get(name))
        assertType(Term.call(name), Sort.Int)
                
    }
}