/**
 * Developed by PuzzleDev s.n.c.
 * 2012 all rights reserved.
 * www.puzzledev.com
 */
package it.polimi.nuzot.core

import scala.collection.mutable.Map

/**
 * @author Michele Sama (m.sama@puzzledev.com)
 *
 * Represent an entry scope to be used with the push and pop paradigm
 */
class Scope[K, T](val parent: Scope[K, T]) {
	private val map = Map[K, T]()
	
	def this() = this(null)
  
    def push(): Scope[K, T] = new Scope(this)
	def pop(): Option[Scope[K, T]] = Option(parent)
	
	def get(key: K): Option[T] = {
	    val x = map.get(key)
	    x match {
	        case Some(v) => {
	            return x
	        }
	        case None => {
	            if (parent == null) {
		            return None;
		        } else {
		            return parent.get(key)
		        }
	        }
	    }
	}
	
	def set(key: K, value: T) = {
	    map(key) = value
	}
}