package it.polimi.nuzot.smt

import org.junit._
import Assert._

import it.polimi.nuzot.smt.grammar._

class SMTParserTestCase {

    val parser = new SMTParser()
    
    @Test
    def testParseInteger = {
        val num = 1234
        var res = parser.parseAll(parser.integer, "" + num)
        assertEquals(num, res.getOrElse())
    }
    
    @Test
    def testParseDouble = {
        val num = -1.234e-123d
        var res = parser.parseAll(parser.real, "" + num)
        assertEquals(num, res.getOrElse())
    }
    
    @Test
    def testParseString = {
        val foo = "\"foo\""
        var res = parser.parseAll(parser.string, foo)
        assertEquals(foo, res.getOrElse())
    }
    
    @Test
    def testParseSymbol = {
        val foo = "foo1"
        var res = parser.parseAll(parser.symbol, foo)
        assertEquals(Symbol(foo).toString(),
                res.getOrElse().toString())
    }
    
    @Test
    def testParseKeyword = {
        val foo = ":foo"
        var res = parser.parseAll(parser.keyword, foo)
        assertEquals(Keyword(foo), res.getOrElse())
    }
    
    @Test
    def testParseSpecStringConst = {
        val foo = "\"foo\""
        var res = parser.parseAll(parser.specConstant, foo)
        assertEquals(SpecStringConstant(foo), res.getOrElse())
    }
    
    @Test
    def testParseSpecIntConst = {
        val num = 123
        val res = parser.parseAll(parser.specConstant, "" + num)
        assertEquals(SpecIntConstant(num), res.getOrElse())
    }
    
    @Test
    def testParseSpecDoubleConst = {
        val num: Double = 5.0
        val res = parser.parseAll(parser.specConstant, "" + num)
        assertEquals(SpecDoubleConstant(num), res.getOrElse())
    }

    
    @Test
    def testParseSpecBooleanConstTrue = {
        val foo = "true"
        var res = parser.parseAll(parser.specConstant, foo).getOrElse(null)
        assertNotNull(res)
        res.value match {
            case x: Boolean => {
                // Ok
            }
            case _ => {
                fail("'true' should be a boolean ")
            }
        }
    }
    
    @Test
    def testParseSpecBooleanConstFalse = {
        val foo = "false"
        var res = parser.parseAll(parser.specConstant, foo).getOrElse(null)
        assertNotNull(res)
        res.value match {
            case x: Boolean => {
                // Ok
            }
            case _ => {
                fail("'false' should be a boolean ")
            }
        }
    }
    
    @Test
    def testParseSExpr = {
        var foo = "()"
        var res = parser.parseAll(parser.sExpr, foo)
        assertEquals(SExprMulti(), res.getOrElse())
        foo = "( 1 asd :bar)"
        res = parser.parseAll(parser.sExpr, foo)
        assertEquals(
                SExprMulti( 
                		SExprSpecConstant(SpecIntConstant(1)), 
                        SExprSymbol(Symbol("asd")),
                        SExprKeyword(Keyword(":bar"))
                		).toString(), res.getOrElse().toString())
    }
    
    @Test
    def testParseIdentifier = {
        var foo = "foo"
        var res = parser.parseAll(parser.identifier, foo)
        assertEquals(foo, res.getOrElse().toString())
        
        foo = "( _foo 1.0 2.0 -23E7)"
        res = parser.parseAll(parser.identifier, foo)
        assertEquals(
                IdentifierSymbolNum(
                        Symbol("foo"),
                        List(1, 2, -23E7f)).toString(), 
                res.getOrElse().toString())
    }
    
    @Test
    def testParseAttributeValue = {
        var foo = "foo"
        var res = parser.parseAll(parser.attributeValue, foo)
        assertEquals(foo, res.getOrElse().toString())
        
        foo = "5"
        res = parser.parseAll(parser.attributeValue, foo)
        assertEquals(foo, res.getOrElse().toString())
        
        foo = "(5 (foo 4) (bar 234 3 asd) )"
        res = parser.parseAll(parser.attributeValue, foo)
        assertEquals(
                AttributeValueSExpr(
                		parser.parseAll(parser.sExpr, "5").getOrElse(null),
                		parser.parseAll(parser.sExpr, "(foo 4)").getOrElse(null),
                		parser.parseAll(parser.sExpr, "(bar 234 3 asd)").getOrElse(null)
                		).toString(), 
                res.getOrElse().toString())
    }
    
    @Test
    def testParseAttribute = {
        var foo = ":foo"
        var res = parser.parseAll(parser.attribute, foo)
        assertEquals(foo, res.getOrElse().toString())
        
        foo = ":foo bar"
        res = parser.parseAll(parser.attribute, foo)
        assertEquals(
                AttributeKeyVal(
                        Keyword(":foo"),
                        AttributeValueSymbol(Symbol("bar"))
                ).toString(),
                res.getOrElse().toString())
        
        foo = ":foo (5 (foo 4) (bar 234 3 asd) )"
        res = parser.parseAll(parser.attribute, foo)
        assertEquals(
                AttributeKeyVal(
                        Keyword(":foo"),
                        AttributeValueSExpr(
                        		parser.parseAll(parser.sExpr, "5").getOrElse(null),
                        		parser.parseAll(parser.sExpr, "(foo 4)").getOrElse(null),
                        		parser.parseAll(parser.sExpr, "(bar 234 3 asd)").getOrElse(null)
	                			) 
       
                ).toString(),
                res.getOrElse().toString())
    }
    
    @Test
    def testParseSort = {
        val foo = "foo"
        var res = parser.parseAll(parser.sort, foo)
        assertEquals(foo, res.getOrElse().toString())
        
        val bar = "( foo (foo foo) )"
        res = parser.parseAll(parser.sort, bar)
        assertEquals(
                SortParametric(
                        IdentifierSymbol(Symbol(foo)),
                        SortParametric(
                                IdentifierSymbol(Symbol(foo)),  
                                SortIdentifier(IdentifierSymbol(Symbol(foo)))
                              	)
                ).toString(), 
                res.getOrElse().toString())
    }
    
    @Test
    def testParseVarBinding = {
        var foo = "( foo 5 )"
        var res = parser.parseAll(parser.varBinding, foo)
        assertEquals(foo, res.getOrElse().toString())
        
    }
    
    @Test
    def testParseSortSymbolDecl = {
        var foo = "( foo 5 )"
        var res = parser.parseAll(parser.sortSymbolDecl, foo)
        assertEquals(foo, res.getOrElse().toString())
        
        foo = "( foo 5 :bar :asd :lol ll )"
        res = parser.parseAll(parser.sortSymbolDecl, foo)
        assertEquals(foo, res.getOrElse().toString())
    }
    
    @Test
    def testParseMetaSpecConstant = {
        var foo = "5"
        var res = parser.parseAll(parser.metaSpecConstant, foo)
        assertEquals(foo, res.getOrElse().toString())
        
        foo = "0.1E-1"
        res = parser.parseAll(parser.metaSpecConstant, foo)
        assertEquals("0.01", res.getOrElse().toString())
        
        foo = "\"foo\""
        res = parser.parseAll(parser.metaSpecConstant, foo)
        assertEquals(foo, res.getOrElse().toString())
    }
    
    @Test
    def testParseFunSymbolDecl = {
        var foo = "(\"foo\" foo :foo)"
        var res = parser.parseAll(parser.funSymbolDecl, foo)
        assertEquals(foo, res.getOrElse().toString())
        
        foo = "(5 foo :foo)"
        res = parser.parseAll(parser.funSymbolDecl, foo)
        assertEquals(foo, res.getOrElse().toString())
        
        foo = "(foo foo foo :foo :foo)"
        res = parser.parseAll(parser.funSymbolDecl, foo)
        assertEquals(foo, res.getOrElse().toString())
    }
    
    @Test
    def testParseParFunSymbolDecl = {
        var foo = "(\"foo\" foo :foo)"
        var res = parser.parseAll(parser.parFunSymbolDecl, foo)
        assertEquals(foo, res.getOrElse().toString())
        
        foo = "(par (foo bar) (foo bar (bar foo) :foo :foo))"
        res = parser.parseAll(parser.parFunSymbolDecl, foo)
        assertEquals(foo, res.getOrElse().toString())
    }
    
      @Test
    def testParseTermKnownFunctionInvocation = {
        var foo = "(div x 4)"
        var res = parser.parseAll(parser.term, foo).getOrElse(null)
        assertNotNull(res)
        res match {
            case x: KnownFunctionInvocation => {
                // Ok
            }
            case _ => {
                fail("div should have been a known function.")
            }
        }
    }
    
    @Test
    def testParseTheoryAttribute = {
        
    }
    
    @Test
    def testParseTheoryDecl = {}
    
    @Test
    def testParseLogicAttribute = {}
    
    @Test
    def testParseLogic = {}
    
    @Test
    def testParseCommand = {}
    
    @Test
    def testParseComment = {
        var commentEndline = "; my meaningful comment <=.,,<x<z!\"£!$%&/&I(O())\n"
        var res = parser.parseAll(parser.comment, commentEndline)
        assertEquals(commentEndline, res.getOrElse())
        commentEndline =
            	";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n"
        res = parser.parseAll(parser.comment, commentEndline)
        assertEquals(commentEndline, res.getOrElse())
        var script = parser.parseAll(parser.script, commentEndline)
        assertEquals(new Script(), script.getOrElse())
        
        val command = "(echo \"foo\")"
        val commandCommentCommand = command + "; comment\n" + command
        script = parser.parseAll(parser.script, commandCommentCommand)
        assertEquals(command + "\n" + command, script.getOrElse().toString())
    }
    
    @Test
    def testParseScript = {
        val foo = "(declare-fun x (Int) Bool)\n" +
        		"(declare-fun y (Int) Bool)\n" +
        		"(assert (= (x 1) true))\n" +
        		"(assert (= (x 1) (y 1)))\n" +
        		"(assert (= (x 2) false))\n" +
        		"(assert (= (x 2) (y 2)))\n" +
        		"(check-sat)\n" +
        		"(get-model)\n" +
        		"(exit)"
        var res = parser.parseAll(parser.script, foo)
        assertEquals(foo, res.getOrElse().toString())
        
        val command = "(declare-fun x (Int) Bool)"
        val comment = "; asd asd"
        val script = command + " " + comment + "\n"
        res = parser.parseAll(parser.script, script)
        assertEquals(command, res.getOrElse().toString())
        
        val multipleComments = 
            	"(assert (= r1 (div a 4))) ; integer division\n" +
        		"(assert (= r2 (mod a 4))) ; mod"
        res = parser.parseAll(parser.script, script)
        assertNotNull(res.getOrElse(null))
    }
}