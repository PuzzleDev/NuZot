/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.smt


import scala.collection.immutable.Queue
import scala.util.parsing.combinator.JavaTokenParsers
import it.polimi.nuzot.smt.grammar._
import it.polimi.nuzot.smt.grammar.CommandGetModel

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 * SMT-lib parser
 */
class SMTParser extends JavaTokenParsers {
    
    def integer: Parser[Int] = {
        wholeNumber ^^ {
            x => x.toInt
        }
    }
    
    def real: Parser[Double] = {
        floatingPointNumber ^^ {
            x => x.toDouble
        }
    }
    
    def number: Parser[AnyVal] = {
        floatingPointNumber ^^ {
            x => {
                try {
                    x.toInt
                } catch {
                    case ex: NumberFormatException => {
                        x.toDouble
                    }
                }
            }
        }
    }
    
    def bool: Parser[Boolean] = {
        "true" ^^ {
            x => true
        } |
        "false" ^^ {
            x => false
        }
    }
    
    
    def string: Parser[String] = {
        stringLiteral
    }
    
    def symbol: Parser[Symbol] = {
        Symbol.pattern ^^ {
            	s => Symbol(s)
            }
    }
    
    def keyword: Parser[Keyword] = {
        Keyword.pattern ^^ {
            	k => Keyword(k)
        	}
    }
    
    def specConstant: Parser[SpecConstant] = {
        number ^^ {
            x => {
                x match {
                    case i: Int => {
                    	SpecIntConstant(i)
                    }
                    case d: Double => {
                    	SpecDoubleConstant(d)
                    }
                    case _ => {
                        throw new IllegalArgumentException(
                                "Number format not supported: " + x)
                    }
                }
            }
        } |
        bool ^^ {
            x => SpecBooleanConstant(x)
        } |
        string ^^ {
            x => SpecStringConstant(x)
        }
    }
        
    
    def sExpr: Parser[SExpr] = {
        specConstant ^^ { 
            x => SExprSpecConstant(x)
        } | 
        symbol  ^^ {
            x => SExprSymbol(x)
        } | 
        keyword  ^^ {
            x => SExprKeyword(x)
        } |
        "(" ~> rep(sExpr) <~ ")" ^^ { 
            x => SExprMulti(x: _*)
        }
    }
    
    def identifier: Parser[Identifier] = {  
        symbol ^^ {
            x => IdentifierSymbol(x)
        } |
        "(" ~ "_" ~> symbol ~ rep1(integer) <~ ")" ^^ { 
            x => IdentifierSymbolNum(x._1, x._2)
        } |
        "(" ~ "_" ~> symbol ~ rep1(real) <~ ")" ^^ { 
            x => IdentifierSymbolNum(x._1, x._2)
        }
    }
    
    def attributeValue: Parser[AttributeValue] = {
        specConstant ^^ { x => AttributeValueSpecConst(x) } |
        symbol ^^ { x => AttributeValueSymbol(x) } |
        "(" ~> rep(sExpr) <~ ")" ^^ { 
            	x => AttributeValueSExpr(x: _*)
        		}
    }
    
    def attribute: Parser[Attribute] = {
        keyword ~ attributeValue ^^ {
            x => AttributeKeyVal(x._1, x._2)
        } |
        keyword ^^ {
            x => AttributeKey(x)
        }
    }
    
    def sort: Parser[Sort] = {
        "(" ~> identifier ~ rep1(sort) <~ ")" ^^ {
            x => SortParametric(x._1, x._2: _*)
        } |
        identifier ^^ {
            x => SortIdentifier(x)
        }
    }
    
    def qualIdentifier: Parser[QualIdentifier] = {
        "(" ~ "as" ~> identifier ~ sort <~ ")" ^^ {
            x => QualIdentifierAs(x._1, x._2)
        } |
        identifier ^^ { x => QualIdentifierIdentifier(x) }
    }
    
    def sortedVar: Parser[SortedVar] = {
        "(" ~> symbol ~ sort <~ ")" ^^ {
            x => SortedVar(x._1, x._2)
        } 
    }
    
     def varBinding: Parser[VarBinding] = {
        "(" ~> symbol ~ term <~ ")" ^^ {
            x => VarBinding(x._1, x._2)
        } 
    }
    
    def term: Parser[Term] = {
        specConstant ^^ { x => TermConst(x) } |
		qualIdentifier ^^ { x => TermQualIdentifierTerms(x) } |
		// Known operators
		"(" ~ "+" ~> rep1(term) <~ ")" ^^ {
            x => Add(x: _*)
        } |
        "(" ~ "and" ~> rep1(term) <~ ")" ^^ {
            x => And(x: _*)
        } |
        "(" ~ "/" ~> term ~ term <~ ")" ^^ {
            x => Div(x._1, x._2)
        } |
        "(" ~ "div" ~> term ~ term <~ ")" ^^ {
            x => Div(x._1, x._2)
        } |
        "(" ~ "mod" ~> term ~ term <~ ")" ^^ {
            x => Mod(x._1, x._2)
        } |
        "(" ~ "rem" ~> term ~ term <~ ")" ^^ {
            x => Rem(x._1, x._2)
        } |
        "(" ~ "=" ~> term ~ term <~ ")" ^^ {
            x => EQ(x._1, x._2)
        } |
        "(" ~ ">=" ~> term ~ term <~ ")" ^^ {
            x => GE(x._1, x._2)
        } |
        "(" ~ ">" ~> term ~ term <~ ")" ^^ {
            x => GT(x._1, x._2)
        } |
        "(" ~ "ite" ~> term ~ term ~ term <~ ")" ^^ {
            x => ITE(x._1._1, x._1._2, x._2)
        } |
        "(" ~ "iff" ~> term ~ term <~ ")" ^^ {
            x => IFF(x._1, x._2)
        } |
        "(" ~ "->" ~> term ~ term <~ ")" ^^ {
            x => IMP(x._1, x._2)
        } |
        "(" ~ "=>" ~> term ~ term <~ ")" ^^ {
            x => IMP(x._1, x._2)
        } |
        "(" ~ "<=" ~> term ~ term <~ ")" ^^ {
            x => LE(x._1, x._2)
        } |
        "(" ~ "<" ~> term ~ term <~ ")" ^^ {
            x => LT(x._1, x._2)
        } |
        "(" ~ "*" ~> rep1(term) <~ ")" ^^ {
            x => Mul(x: _*)
        } |
        "(" ~ "!" ~> term <~ ")" ^^ {
            x => Not(x)
        } |
        "(" ~ "not" ~> term <~ ")" ^^ {
            x => Not(x)
        } |
        "(" ~ "or" ~> rep1(term) <~ ")" ^^ {
            x => Or(x: _*)
        } |
        "(" ~ "-" ~> rep1(term) <~ ")" ^^ {
            x => Sub(x: _*)
        } |
        "(" ~ "Xor" ~> term ~ term <~ ")" ^^ {
            x => Xor(x._1, x._2)
        } |
		// Qual identifiers
		"(" ~> qualIdentifier ~ rep1(term) <~ ")" ^^ {
            x => TermQualIdentifierTerms(x._1, x._2: _*)
        } |
        // Deprecated
		"(" ~ "let" ~ "(" ~> rep1(varBinding) ~
				")" ~ term <~ 
				")" ^^ {
		    // TODO remove this
            x => TermLet(x._1._1, x._2)
        } |
        // Deprecated
		"(" ~ "forall" ~ "(" ~> rep1(sortedVar) ~ 
				")" ~ term <~ 
				")" ^^ {
		    // TODO remove this
            x => TermForAll(x._1._1, x._2)
        } |
        // Deprecated
		"(" ~ "exists" ~ "(" ~> rep1(sortedVar) ~ 
				")" ~ term <~ 
				")" ^^ {
		    // TODO remove this
            x => TermExists(x._1._1, x._2)
        } |
        // Deprecated
		"(" ~ "!" ~ "(" ~> term ~ 
				")" ~ rep1(attribute) <~ 
				")" ^^ {
		    // TODO remove this
            x => TermNotTerm(x._1._1, x._2)
        } 
    }
    
    def sortSymbolDecl: Parser[SortSymbolDecl] = {
    	"(" ~> identifier ~ integer ~ rep(attribute) <~ ")" ^^ {
            x => SortSymbolDecl(x._1._1, x._1._2, x._2)
        } |
        "(" ~> identifier ~ real ~ rep(attribute) <~ ")" ^^ {
            x => SortSymbolDecl(x._1._1, x._1._2, x._2)
        }
    }
    
    def metaSpecConstant: Parser[MetaSpecConst] = {
    	number ^^ {
            x => {
                x match {
                    case i: Int => {
                    	MetaSpecIntConst(i)
                    }
                    case d: Double => {
                    	MetaSpecDoubleConst(d)
                    }
                    case _ => {
                        throw new IllegalArgumentException(
                                "Number format not supported: " + x)
                    }
                }
            }
        } |
    	string ^^ {
    	    x => MetaSpecStringConst(x) 
    	}
    }

    def funSymbolDecl: Parser[FunSymbolDecl] = {
    	"(" ~> specConstant ~ sort ~ rep(attribute) <~ ")" ^^ {
            x => FunSymbolDeclSpecConst(x._1._1, x._1._2, x._2)
        } |
        "(" ~> metaSpecConstant ~ sort ~ rep(attribute) <~ ")" ^^ {
            x => FunSymbolDeclMetaSpecConst(x._1._1, x._1._2, x._2)
        } |
        "(" ~> identifier ~ rep1(sort) ~ rep(attribute) <~ ")" ^^ {
            x => FunSymbolDeclIdentifier(x._1._1, x._1._2, x._2)
        }
    }
    
    def parFunSymbolDecl: Parser[ParFunSymbolDecl] = {
        funSymbolDecl ^^ { x => ParFunSymbolDeclFunSymbol(x) } |
		"(" ~ "par" ~ "(" ~> rep1(symbol) ~ ")" ~ "(" ~
			identifier ~ rep1(sort) ~ rep(attribute) <~ ")" ~ ")" ^^ {
		    x  => ParFunSymbolDeclPar(x._1._1._1._1._1, x._1._1._2, x._1._2, x._2)
		}
    }
    
    def theoryAttribute: Parser[TheoryAttribute] = {
        attribute ^^ { x => TheoryAttribute.attribute(x)} |
        ":notes" ~> string ^^ { x => TheoryAttribute.notes(x) } |
        ":values" ~> string ^^ { x => TheoryAttribute.values(x) } |
        ":definition" ~> string ^^ { x => TheoryAttribute.definition(x) } |
        ":funs-description" ~> string ^^ { x => TheoryAttribute.funsDescription(x) } |
        ":sorts-description" ~> string ^^ { x => TheoryAttribute.sortsDescription(x) } |
        ":funs" ~ "(" ~> rep1(parFunSymbolDecl) <~ ")" ^^ { x => TheoryAttribute.funs(x) } |
        ":sort" ~ "(" ~> rep1(sortSymbolDecl) <~ ")" ^^ { x => TheoryAttribute.sort(x) }
    }
    
    def theoryDecl: Parser[TheoryDecl] = {
        "(" ~ "theory"  ~> symbol ~ rep1(theoryAttribute) <~ ")" ^^ {
            x => TheoryDecl(x._1, x._2)
        }
    }
    
    def logicAttribute: Parser[LogicAttribute] = {
        ":theories" ~ "(" ~> rep1(symbol) <~ ")" ^^ {
            x => LogicAttribute.theories(x)
        } |
        ":language" ~> string  ^^ {
            x => LogicAttribute.language(x)
        } |
        ":extensions" ~> string  ^^ {
            x => LogicAttribute.extensions(x)
        } |
        ":values" ~> string  ^^ {
            x => LogicAttribute.values(x)
        } |
        ":notes" ~> string  ^^ {
            x => LogicAttribute.notes(x)
        } |
        attribute ^^ {
            x => LogicAttribute.attribute(x)
        }
    }
    
    def logic: Parser[Logic] = {
        "(" ~ "logic" ~> symbol ~ rep1(logicAttribute) <~ ")" ^^ {
        	x => Logic(x._1, x._2: _*)
        }
    }
    
    def command: Parser[Command] = {
        "(" ~ "set-logic" ~> symbol <~ ")" ^^ {
            x => InitCommandSetLogic(x)
        } |
        "(" ~ "set-info" ~> attribute <~ ")" ^^ {
            x => InitCommandSetInfo(x)
        } |
        "(" ~ "assert" ~> term <~ ")" ^^ {
            x => CommandAssert(x)
        } |
        "(" ~ "check-sat" ~ ")" ^^ {
            x => CommandCheckSat()
        } |
        "(" ~ "get-model" ~ ")" ^^ {
            x => CommandGetModel()
        } |
        "(" ~ "declare-sort" ~> symbol ~ integer <~ ")" ^^ {
            x => CommandDeclareSort(x._1, x._2)
        } |
        "(" ~ "declare-sort" ~> symbol ~ real <~ ")" ^^ {
            x => CommandDeclareSort(x._1, x._2)
        } |
        "(" ~ "define-sort" ~> symbol ~ "(" ~ rep(symbol) ~ ")" ~ sort <~ ")" ^^ {
            x => CommandDefineSort(x._1._1._1._1, x._1._1._2, x._2)
        } |
        "(" ~ "declare-fun" ~> symbol ~ "(" ~ rep(sort) ~ ")" ~ sort <~ ")" ^^ {
            x => CommandDeclareFun(x._1._1._1._1, x._1._1._2, x._2)
        } |
        "(" ~ "declare-const" ~> symbol ~ sort <~ ")" ^^ {
            // Syntactic sugar
            // A constant is simply a function with no args 
            x => CommandDeclareFun(x._1, List(), x._2) 
        } |
        "(" ~ "define-fun" ~> symbol ~ "(" ~ rep(sortedVar) ~ ")" ~ sort ~ term <~ ")" ^^ {
            x => CommandDefineFun(
                    x._1._1._1._1._1, 
                    x._1._1._1._2,
                    x._1._2, x._2)
        } |
        "(" ~ "echo" ~> string <~ ")" ^^ {
            x => CommandEcho(x)
        } |
        "(" ~ "push" ~ ")" ^^ {
            x => CommandPush()
        } |
        "(" ~ "pop" ~ ")" ^^ {
            x => CommandPop()
        } |
        "(" ~ "exit" ~ ")" ^^ {
            x => CommandExit()
        }
    }
    
    def comment: Parser[String] = {
        """;.*\n""".r
    }
    
    def script: Parser[Script] = {
        rep("""[ ]*\n""".r | command | comment) ^^ {
            x => {
                var script = new Script()
                x.foreach(_ match {
                    case c: Command => {
                        script = script :+ c
                    }
                    case x => {
                        println("skipping" + x)
                        // Skip comments and new lines
                    }
                })
                script
            }
        }
    }
    
    def skipComment(string: String) : String = {
        return string.split(";")(0)
    }
}