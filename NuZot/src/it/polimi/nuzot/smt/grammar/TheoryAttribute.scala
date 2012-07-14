/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.smt.grammar

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 * <theory_attribute> ::== :sort ( <sort_symbol>+ ) // <sort_symbol> is not defined. Maybe they meant <sort> or <sort_symbol_decl>
 *		| :funs ( <par_fun_symbol_decl>+ )
 *		| :sorts-description <string>
 *		| :funs-description <string>
 *		| :definition <string>
 *		| :values <string>
 *		| :notes <string>
 *		| <attribute>
 */
object TheoryAttribute {
    
    def sort(symbols: List[SortSymbolDecl]): TheoryAttribute = {
        return TheoryAttribute(Option(Keyword(":sort")), symbols)
    }
    def funs(symbols: List[ParFunSymbolDecl]): TheoryAttribute = {
        return TheoryAttribute(Option(Keyword(":funs")), symbols)
    }
    def sortsDescription(string: String): TheoryAttribute = {
        return TheoryAttribute(Option(Keyword(":sorts-description")), string)
    }
    def funsDescription(string: String): TheoryAttribute = {
        return TheoryAttribute(Option(Keyword(":funs-description")), string)
    }
    def definition(string: String): TheoryAttribute = {
        return TheoryAttribute(Option(Keyword(":definition")), string)
    }
    def values(string: String): TheoryAttribute = {
        return TheoryAttribute(Option(Keyword(":values")), string)
    }
    def notes(string: String): TheoryAttribute = {
        return TheoryAttribute(Option(Keyword(":notes")), string)
    }
    def attribute(attribute: Attribute): TheoryAttribute = {
        return TheoryAttribute(null, attribute)
    }
}

case class TheoryAttribute(val keyword: Option[Keyword], val value: Any)
		extends Statement {
    override def toString(): String = {
        (keyword, value) match {
            case (k: Keyword, v: List[SortSymbolDecl]) => {
                return new StringBuilder()
                	.append(k)
                	.append(" ( ")
                	.append(v.mkString(" "))
                	.append(" )")
                	.toString()
            }
            case (k: Keyword, v: List[ParFunSymbolDecl]) => {
                return new StringBuilder()
                	.append(k)
                	.append(" ( ")
                	.append(v.mkString(" "))
                	.append(" )")
                	.toString()
            }
            case (k: Keyword, v: String) => {
                return k + " " + v
            }
            case (null, v: Attribute) => {
                return v.toString()
            }
        }
    }
}