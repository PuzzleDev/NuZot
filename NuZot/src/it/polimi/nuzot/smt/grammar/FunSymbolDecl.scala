/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.smt.grammar

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 * <fun_symbol_decl> ::== ( <spec_constant> <sort> <attribute>* )
 *		| ( <meta_spec_constant> <sort> <attribute>* )
 *		| ( <identifier> <sort>+ <attribute>* )
 *
 */
trait FunSymbolDecl extends Statement


case class FunSymbolDeclSpecConst(
        val const: SpecConstant,
        val sort: Sort, 
        val attributes: List[Attribute])
        extends FunSymbolDecl {
    
    override def toString(): String = {
        val builder = new StringBuilder("(")
        builder.append(const.toString())
        		.append(" ")
        		.append(sort.toString())
        
        if (attributes != List()) {
            builder.append(" ")
        	    	.append(attributes.mkString(" "))
        }
        return builder.append(")")
        	.toString()    
    }
}


case class FunSymbolDeclMetaSpecConst(
        val const: MetaSpecConst,
        val sort: Sort, 
        val attributes: List[Attribute])
        extends FunSymbolDecl {
    
    override def toString(): String = {
        val builder = new StringBuilder("(")
        builder.append(const.toString())
        		.append(" ")
        		.append(sort.toString())
        
        if (attributes != List()) {
            builder.append(" ")
        	    	.append(attributes.mkString(" "))
        }
        return builder.append(")")
        	.toString()    
    }
}


case class FunSymbolDeclIdentifier(
        val identifier: Identifier,
        val sorts: List[Sort], 
        val attributes: List[Attribute])
        extends FunSymbolDecl {
    
    override def toString(): String = {
        val builder = new StringBuilder("(")
        builder.append(identifier.toString())
        		.append(" ")
        		.append(sorts.mkString(" "))
        
        if (attributes != List()) {
            builder.append(" ")
        	    	.append(attributes.mkString(" "))
        }
        return builder.append(")")
        	.toString()    
    }
}