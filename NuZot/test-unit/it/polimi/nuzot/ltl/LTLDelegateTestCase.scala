/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.ltl

import org.junit._
import Assert._

import it.polimi.nuzot.ltl.grammar._
import it.polimi.nuzot.smt.grammar._

/**
 *  @author Michele Sama (m.sama@puzzledev.com)
 *	
 *  Test runtime delegate assignment.
 */
class LTLDelegateTestCase {

    var interpreter: LTLInterpreter = null
    
    @Before
    def setUp(): Unit = {
        interpreter = new LTLInterpreter()
    }
    
    def assertLogics(logicName: String): Unit = {
        assertEquals(
            "Logic has not been loaded correctly",
            List(Symbol(logicName)),
            interpreter.logics
            )
    }
    
    def createScript(logicName: String): Script = {
        return Script(List()) :+ 
        		InitCommandSetLogic(Symbol(logicName)) :+
        		InitCommandSetInfo(
        		        AttributeKeyVal(
        		                LTLInterpreter.kKeyword,
        		                AttributeValueSpecConst(SpecIntConstant(1)))) :+
        		CommandEcho("eco")
    }
    
    @Test
    def testSetAritmeticDelegate(): Unit = {
        val delegateClass: String = classOf[FakeArithmeticDelegate].getName()
        
        interpreter.doVisit(createScript(delegateClass))
        assertLogics(delegateClass)
        
        interpreter.arithmeticDelegate() match {
            case x: FakeArithmeticDelegate => {
                // pass
            }
            case _ => {
                fail("The arithmetic delegate has not been redefined: " +
                        interpreter.arithmeticDelegate)
            }
        }
    }
    
    @Test
    def testSetLogicDelegate(): Unit = {
        val delegateClass: String = classOf[FakeLogicDelegate].getName()
        
        interpreter.doVisit(createScript(delegateClass))
        assertLogics(delegateClass)
        
        interpreter.logicDelegate() match {
            case x: FakeLogicDelegate => {
                // pass
            }
            case _ => {
                fail("The logic delegate has not been redefined: " +
                        interpreter.logicDelegate())
            }
        }
    }
    
    @Test
    def testSetEqualityDelegate(): Unit = {
        val delegateClass: String = classOf[FakeEqualityDelegate].getName()
        
        interpreter.doVisit(createScript(delegateClass))
        assertLogics(delegateClass)
        
        interpreter.equalityDelegate() match {
            case x: FakeEqualityDelegate => {
                // pass
            }
            case _ => {
                fail("The equality delegate has not been redefined: " +
                        interpreter.equalityDelegate())
            }
        }
    }
}

class FakeArithmeticDelegate extends ArithmeticDelegate {
    def generatePreconditions(ltl: LTLInterpreter): Script = {
        return new Script()
    }
    
    def expandArithmeticTemporalOperator(
            ltl: LTLInterpreter,
            term: ArithmeticTemporalOperator,
            computed: Script): Script = {
    	return computed	
    }
        
    def expandArithmeticOperator(
            ltl: LTLInterpreter,
            term: ArithmeticOperator,
            computed: Script): Script = {
        return computed
    }        
}

class FakeLogicDelegate extends LogicDelegate {
    def generatePreconditions(ltl: LTLInterpreter): Script = {
        return new Script()
    }
    
    def expandTemporalBooleanOperator(
            ltl: LTLInterpreter,
            term: TemporalOperator,
            computed: Script): Script = {
        return computed
    }
        
    def expandBooleanOperator(
            ltl: LTLInterpreter,
            term: BooleanOperator,
            computed: Script): Script = {
        return computed
    }
}

class FakeEqualityDelegate extends EqualityDelegate {
    def generatePreconditions(ltl: LTLInterpreter): Script = {
        return new Script()
    }
    
    def expandEqualityOperator(
	        ltl: LTLInterpreter,
	        term: EqualityOperator,
	        computed: Script): Script = {
    	return computed
    }
}
