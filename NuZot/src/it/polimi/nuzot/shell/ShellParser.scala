/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.shell

import it.polimi.nuzot.shell.grammar.Command
import it.polimi.nuzot.smt.SMTParser
import it.polimi.nuzot.smt.grammar.{Command => SMTCommand, Script}
import it.polimi.nuzot.ltl.LTLParser
import scala.io.Source

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 * Parser for the shell command. This should be the
 * leaf class of the parser hierarchy and will filter
 * all the shell commands from the real solver instructions.
 * 
 * 
 */
sealed class ShellParser extends LTLParser {

	override def command: Parser[SMTCommand] = {
        "(" ~ Command.saveLabel ~> string <~ ")" ^^ {
            x => Command.save(x)
        } |
        "(" ~ Command.loadLabel ~> string <~ ")" ^^ {
            x => Command.load(x)
        } |
        super.command
    }
    
	def loadFile(filename: String): Script = {
	    val builder = new StringBuilder()
	    var loadedScript = new Script()
	    println("loading: " + filename)
	    Source.fromFile(filename).getLines().foreach(line => {
	        val strippedLine = if (line.contains(";")) {
	            """;""".r.split(line) match {
	                case Array() => {
	                    ""
	                }
	                case x => {
	                    x(0)
	                }
	            } 
	        } else {
	             line
	        }.trim()
	        
	        val parsedString = builder.append(strippedLine).toString()
	        if (parsedString.count(x => x == '(') == 
	            	parsedString.count(x => x == ')')) {
	            // The command is complete
		        val script = parseAll(
		                this.script, parsedString)
		                .getOrElse(null)
		        builder.clear()
		        if (script != null) {
	            	loadedScript = loadedScript ++ script
		        } else {
		            throw new IllegalArgumentException("Parse error: "  + parsedString)
		        }
	        }
	    })
	    return loadedScript
	}
}