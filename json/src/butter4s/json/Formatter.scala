/*
 * The MIT License
 *
 * Copyright (c) 2010 Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package butter4s.json

import scala.collection.mutable.StringBuilder
import scala.annotation.switch

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */
object Formatter {
	def format( json: String ) = {
		val b = new StringBuilder
		var string = false
		var escape = false
		var tabs = new StringBuilder
		for ( c <- json ) ( c: @switch ) match {
			case '"' if !escape => b.append( "\"" ); string = !string; escape = false
			case x@( '{' | '[' ) if !string => tabs += '\t'; b.append( x ).append( "\n" ).append( tabs ); escape = false
			case x@( '}' | ']' ) if !string => tabs.deleteCharAt( tabs.length - 1 ); b.append( "\n" ).append( tabs ).append( x ); escape = false
			case x@',' if !string => b.append( x ).append( "\n" ).append( tabs ); escape = false
			case x@'\\' if string => b.append( x ); escape = !escape
			case x@':' if !string => b.append( x ).append( " " ); escape = false
			case '\n' | '\r' | '\t' | ' ' if !string =>
			case x => b.append( x ); escape = false
		}
		b.toString
	}
}