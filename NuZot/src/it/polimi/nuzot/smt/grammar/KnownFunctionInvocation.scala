/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.smt.grammar
import z3.scala._

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 * Defines the function call of a known function.
 * Known operators are parsed as instance of this
 * trait instead of normal terms. This is done to simplify
 * internal conversions.
 * 
 * Any not-user-defined function should extend
 * from this trait.
 */
abstract class KnownFunctionInvocation(val name: String)
		extends Term

trait BooleanOperator extends Term

trait ArithmeticOperator extends Term
		
		
abstract class UnaryOperator(
        override val name: String, val value: Term)
		extends KnownFunctionInvocation(name) {
    
    override def toString(): String = {
        return new StringBuilder("(" + name + " ")
        	.append(value)
        	.append(")")
        	.toString()
    }
    
}
		
abstract class BinaryOperator(
        override val name: String, val value1: Term, val value2: Term)
		extends KnownFunctionInvocation(name)  {
    
    override def toString(): String = {
        return new StringBuilder("(" + name + " ")
        	.append(value1)
        	.append(" ")
        	.append(value2)
        	.append(")")
        	.toString()
    }
    
}

abstract case class EqualityOperator(
        override val name: String, 
        override val  value1: Term, 
        override val  value2: Term)
		extends BinaryOperator(name, value1, value2)


abstract class TernaryOperator(
        override val name: String,
        val value1: Term, val value2: Term, val value3: Term)
		extends KnownFunctionInvocation(name)  {
    
    override def toString(): String = {
        return new StringBuilder("(ite ")
        	.append(value1)
        	.append(" ")
        	.append(value2)
        	.append(" ")
        	.append(value3)
        	.append(")")
        	.toString()
    }
    
}


abstract class MultiOperator(
        override val name: String, val values: Term*)
		extends KnownFunctionInvocation(name) {
    
    override def toString(): String = {
        return new StringBuilder("(" + name + " ")
        	.append(values.mkString(" "))
        	.append(")")
        	.toString()
    }
}

sealed case class Add(override val values: Term*)
		extends MultiOperator("+", values: _*) with ArithmeticOperator


sealed case class And(override val values: Term*)
		extends MultiOperator("and", values: _*) with BooleanOperator


sealed case class Div(nom: Term, denom: Term)
		extends BinaryOperator("/", nom, denom) with ArithmeticOperator


sealed case class Mod(nom: Term, denom: Term)
		extends BinaryOperator("mod", nom, denom) with ArithmeticOperator


sealed case class Rem(left: Term, right: Term)
		extends BinaryOperator("rem", left, right) with BooleanOperator


sealed case class EQ(left: Term, right: Term)
		extends EqualityOperator("=", left, right)


sealed case class GE(val left: Term, val right: Term)
		extends EqualityOperator(">=", left, right)


sealed case class GT(val left: Term, val right: Term)
		extends EqualityOperator(">", left, right)


sealed case class ITE(val ifa: Term, thena: Term, elsea: Term)
		extends TernaryOperator("ite", ifa, thena, elsea) with BooleanOperator


sealed case class IFF(ifa: Term, thena: Term)
		extends BinaryOperator("=", ifa, thena) with BooleanOperator


sealed case class IMP(val ifa: Term, val thena: Term)
		extends BinaryOperator("=>", ifa, thena) with BooleanOperator


sealed case class LE(left: Term, right: Term)
		extends EqualityOperator("<=", left, right)


sealed case class LT(left: Term, right: Term)
		extends EqualityOperator("<", left, right)


sealed case class Mul(override val values: Term*)
		extends MultiOperator("*", values: _*) with ArithmeticOperator


sealed case class Not(override val value: Term)
		extends UnaryOperator("not", value) with BooleanOperator


sealed case class Or(override val values: Term*)
		extends MultiOperator("or", values: _*) with BooleanOperator


sealed case class Sub(override val values: Term*)
		extends MultiOperator("-", values: _*) with ArithmeticOperator


sealed case class Xor(left: Term, right: Term)
		extends BinaryOperator("xor", left, right) with BooleanOperator
