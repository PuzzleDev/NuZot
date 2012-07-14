/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.smt.grammar

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 * 
 * <term> ::== <spec_constant>
 *		| <qual_identifier>
 *  	| ( <qual_identifier> <term>+ )
 *		| ( let ( <var_binding>+ ) <term> ) ;deprecated
 *		| ( forall ( <sorted_var>+ ) <term> ) ;deprecated
 *		| ( exists ( <sorted_var>+ ) <term> ) ;deprecated
 *		| ( ! <term> <attribute>+ ) ;deprecated
 */
object Term {
        	
    def True(): Term = {
        return TermConst(SpecBooleanConstant(true))
    }
    
    def False(): Term = {
        return TermConst(SpecBooleanConstant(false))
    }
    
    /**
     * Syntactic sugar for a parametric function invocation.
     * XXX(m.sama): this part of the SMT-lib grammar is too verbose.
     */
    def call(fzName: String, args: Term*): Term = {
        return call(Symbol(fzName), args: _*)
    }
    
    /**
     * Syntactic sugar for a parametric function invocation.
     */
    def call(fzName: Symbol, args: Term*): Term = {
        return TermQualIdentifierTerms(
                QualIdentifierIdentifier(IdentifierSymbol(fzName)),
                args: _*)
    }
    
    def const(value: Int) : Term = {
        return TermConst(SpecIntConstant(value))
    }
    
    def const(value: Double) : Term = {
        return TermConst(SpecDoubleConstant(value))
    }
    
    def const(value: Boolean) : Term = {
        value match {
            case true => True()
            case false => False()
        }
    }

}


trait Term extends Statement

sealed case class TermConst(val const: SpecConstant)
		extends Term {
    
    override def toString(): String = {
        return const.toString()
    }
 
}

sealed case class TermQualIdentifierTerms(
        val qualIdentifier: QualIdentifier,
        val terms: Term*) extends Term {
    
    override def toString(): String = {
        if (terms.size == 0) {
        	qualIdentifier.toString()    
        } else {
            new StringBuilder("(")
	        		.append(qualIdentifier)
	        		.append(" ")
	            	.append(terms.mkString(" "))
	            	.append(")")
	            	.toString()
        }
    }
}

@Deprecated
sealed case class TermLet(
        var statements: List[Statement],
        var term: Term) extends Term {
    override def toString(): String = {
        return new StringBuilder("(let (")
        	.append(statements.mkString(" "))
        	.append(") ")
        	.append(term)
        	.append(")")
        	.toString()
    }
}

@Deprecated
sealed case class TermForAll(
        val sortedVars: List[SortedVar],
        val body: Term) extends Term {
    override def toString(): String = {
        return new StringBuilder("(forall (")
        	.append(sortedVars.mkString(" "))
        	.append(") ")
        	.append(body)
        	.append(")")
        	.toString()
    }
}

@Deprecated
sealed case class TermExists(
        val statements: List[Statement],
        val term: Term) extends Term {
    override def toString(): String = {
        return new StringBuilder("(exists (")
        	.append(statements.mkString(" "))
        	.append(") ")
        	.append(term)
        	.append(")")
        	.toString()
    }
}

@Deprecated
sealed case class TermNotTerm(
        val term: Term,
        val attributes: List[Attribute]) extends Term {
    override def toString(): String = {
        return new StringBuilder("( ! ( ")
        	.append(term)
        	.append(" ) ")
        	.append(attributes.mkString(" "))
        	.append(" )")
        	.toString()
    }
}