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
package butter4s

import lang.Predicate.cast
import lang.Predicate.P

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */

package object lang {
	implicit def identity[A]( a: A ) = a

	implicit def toPredicate[A]( f: A => Boolean ): P[A] = cast( f )

	def not[A]( f: A => Boolean ) = f.not

	def wrapIf( cond: Boolean )( left: => String, value: Any, right: => String ) = if ( cond ) left + value + right else "" + value

	class RichTuple2[A, B]( t: (A, B) ) {
		def map[C, D]( f: ( (A, B) ) => (C, D) ) = f( t )
	}

	implicit def toRichTuple2[A, B]( t: (A, B) ) = new RichTuple2( t )

	implicit def toRichTraversableOnce[A] = collection.toRichTraversableOnce[A] _

	implicit def toRichArray[A] = collection.toRichArray[A] _

	class RichString( s: String ) {
		def substringAfter( delimiter: String ) = if ( s.contains( delimiter ) ) s.substring( s.indexOf( delimiter ) + 1 ) else ""

		def substringAfterLast( delimiter: String ) = if ( s.contains( delimiter ) ) s.substring( s.lastIndexOf( delimiter ) + 1 ) else ""

		def substringBefore( delimiter: String ) = if ( s.contains( delimiter ) ) s.substring( 0, s.indexOf( delimiter ) ) else s
	}

	implicit def toRichString( s: String ) = new RichString( s )
}