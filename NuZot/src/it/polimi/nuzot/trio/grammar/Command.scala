/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.trio.grammar


import it.polimi.nuzot.smt.grammar.{Command => SMTCommand}

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 *
 *Gli operatori base di LTL che sono implementati nei plugin sono:
- neXt, Yesterday, Zeta
- Until, Release
- Since, Trigger

Per implementati intendo che sono quegli operatori che vengono svolti nel tempo ricorsivamente mediante quelle funzioni tipo genFutr, genPast, etc. nei vari plugin.
Usando questi operatori abbiamo l'espressività di tutta LTL. Dobbiamo però poter esprimere anche la costante TRUE e FALSE.
Quindi, predisponi nel parsing delle formule anche un simbolo o la keyword TRUE (e FALSE).
Questo ci serve, ad esempio, per esprimere l'eventually "qualcosa"  (che trovi indicato nei paper con F ) che è (TRUE Until "qualcosa").
Nota che Zeta si comporta "quasi" come Yesterday e serve perchè è il duale di Yesterday che viene chiamato in uso mentre si denega.


In trio-utils abbiamo poi tutti gli operatori alla Trio derivati.
- AlwaysF (alwf)
- AlwaysP (alep)
- SomewhereF (somf)
- SomewhereP (somp)
- WithinF (withinf)
- WithinP (withinp)

Ce ne sono anche altri ma credo che per ora possiamo pensare a questi.

 *
 *	<trio_command> ::== <command>
 */
class Command extends SMTCommand {

}