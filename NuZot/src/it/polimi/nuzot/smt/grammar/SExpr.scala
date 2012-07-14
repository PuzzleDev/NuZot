/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.smt.grammar

import scala.collection.immutable.Queue

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 * <s_expr> ::= <spec_constant> | <symbol> | <keyword> | ( <s_expr>* )
 * 
 * XXX(m.sama): This is rarely used. It "smells" like a leftover
 * in the SMT-lib grammar. 
 */
trait SExpr extends Statement

sealed case class SExprSpecConstant(val value: SpecConstant)
		extends SExpr {
    
    override def toString(): String = {
        return value.toString()
    }
}

sealed case class SExprSymbol(val value: Symbol)
		extends SExpr {
    
    override def toString(): String = {
        return value.toString()
    }
}

sealed case class SExprKeyword(val value: Keyword)
		extends SExpr {
    
    override def toString(): String = {
        return value.toString()
    }
}

sealed case class SExprMulti(val values: SExpr*)
		extends SExpr { 
    
    override def toString(): String = {
        return new StringBuilder("( ")
        	.append(values.mkString(" "))
        	.append(" )")
        	.toString()
    }
}



