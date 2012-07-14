package it.polimi.nuzot.smt.grammar

import scala.collection.immutable.Queue

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 * <par_fun_symbol_decl> ::== <fun_symbol_decl>
 *		| ( par ( <symbol>+ ) ( <identifier> <sort>+ <attribute>* ) )
 */
trait ParFunSymbolDecl extends Statement

sealed case class ParFunSymbolDeclFunSymbol(val value: FunSymbolDecl)
		extends ParFunSymbolDecl {
    
    override def toString(): String = {
        return value.toString()
    }
}

sealed case class ParFunSymbolDeclPar(
        val symbol: List[Symbol],
        val identifier: Identifier,
        val sorts: List[Sort],
        val attributes: List[Attribute])
		extends ParFunSymbolDecl {
    
    override def toString(): String = {
        new StringBuilder("(par (")
	        	.append(symbol.mkString(" "))
	        	.append(") (")
	        	.append(identifier)
	        	.append(" ")
	        	.append(sorts.mkString(" "))
	        	.append(" ")
	        	.append(attributes.mkString(" "))
	        	.append("))")
	        	.toString()
    }
}