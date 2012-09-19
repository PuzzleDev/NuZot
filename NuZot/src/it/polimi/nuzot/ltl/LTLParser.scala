/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.ltl

import it.polimi.nuzot.smt.SMTParser
import it.polimi.nuzot.smt.grammar.{Command => SMTCommand}
import it.polimi.nuzot.smt.grammar.Symbol
import it.polimi.nuzot.smt.grammar.Term
import it.polimi.nuzot.ltl.grammar.{Command => LTLCommand}
import it.polimi.nuzot.ltl.grammar.CommandDeclareTFun
import it.polimi.nuzot.ltl.grammar.CommandForceILoop
import it.polimi.nuzot.ltl.grammar.CommandTemporalAssert
import it.polimi.nuzot.ltl.grammar.Next
import it.polimi.nuzot.ltl.grammar.Until
import it.polimi.nuzot.ltl.grammar.Release
import it.polimi.nuzot.ltl.grammar.Yesterday
import it.polimi.nuzot.ltl.grammar.Zeta
import it.polimi.nuzot.ltl.grammar.Since
import it.polimi.nuzot.ltl.grammar.Trigger
//MR: added
import it.polimi.nuzot.ltl.grammar.NextV
import it.polimi.nuzot.ltl.grammar.YesterdayV

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 * 
 */
class LTLParser extends SMTParser {

    override def term: Parser[Term] = {
		"(" ~ "next" ~> term <~ ")" ^^ {
            x => Next(x)
        } |
        "(" ~ "until" ~> term ~ term <~ ")" ^^ {
            x => Until(x._1, x._2)
        } |
        "(" ~ "release" ~> term ~ term <~ ")" ^^ {
            x => Release(x._1, x._2)
        } |
        "(" ~ "yesterday" ~> term <~ ")" ^^ {
            x => Yesterday(x)
        } |
        "(" ~ "zeta" ~> term <~ ")" ^^ {
            x => Zeta(x)
        } |
        "(" ~ "since" ~> term ~ term <~ ")" ^^ {
            x => Since(x._1, x._2)
        } |
        "(" ~ "trigger" ~> term ~ term <~ ")" ^^ {
            x => Trigger(x._1, x._2)
        } |
//MR: added to take into account arithmetic temporal terms
        "(" ~ "next_v" ~> term <~ ")" ^^ {
            x => NextV(x)
        } |
        "(" ~ "yesterday_v" ~> term <~ ")" ^^ {
            x => YesterdayV(x)
        } |        
        super.term
    }
    
    override def command: Parser[SMTCommand] = {
        "(" ~ LTLCommand.forceILoop ~> bool <~ ")" ^^ {
            x => CommandForceILoop(x)
        } |
        "(" ~ LTLCommand.temporalAssert ~> term ~ specConstant <~ ")" ^^ {
            x => CommandTemporalAssert(x._1, x._2)
        } |
        // temporal variable / function with arity = 0
        "(" ~ LTLCommand.declareTFun ~> symbol ~  sort <~ ")" ^^ {
            x => CommandDeclareTFun(x._1, List(), x._2)
        } |
        // temporal parametric function
        "(" ~ LTLCommand.declareTFun ~> symbol ~ "(" ~ rep(sort) ~ ")" ~ sort <~ ")" ^^ {
            x => CommandDeclareTFun(x._1._1._1._1, x._1._1._2, x._2)
        } |
        super.command
    }
}