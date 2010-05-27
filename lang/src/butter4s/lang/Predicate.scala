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

package butter4s.lang

import butter4s.lang.Function.F

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */
object Predicate {
	abstract class P[A] extends F[A, Boolean] {
		override def toString() = "<predicate>"

		def and( f: ( _ >: A ) => Boolean ): P[A] = this && f

		def &&( f: ( _ >: A ) => Boolean ): P[A] = new And( this, f )

		def or( f: ( _ >: A ) => Boolean ): P[A] = this || f

		def ||( f: ( _ >: A ) => Boolean ): P[A] = new Or( this, f )

		def not: P[A] = new Not( this )
	}

	private class Not[A]( f: A => Boolean ) extends P[A] {
		def apply( a: A ) = !f( a )

		override def toString() = "!" + f
	}

	private class And[A]( f1: A => Boolean, f2: A => Boolean ) extends P[A] {
		private var ps: List[A => Boolean] = f1 :: f2 :: Nil

		def apply( a: A ) = ps.forall( _( a ) )

		override def &&( f: ( _ >: A ) => Boolean ): P[A] = {ps = f :: ps; this}

		override def toString() = ps.tail.foldLeft( ps.head.toString )( _ + " AND " + _ )
	}

	private class Or[A]( f1: A => Boolean, f2: A => Boolean ) extends P[A] {
		private var ps: List[A => Boolean] = f1 :: f2 :: Nil

		def apply( a: A ) = ps.find( _( a ) ).isDefined

		override def &&( f: ( _ >: A ) => Boolean ): P[A] = {ps = f :: ps; this}

		override def toString() = ps.tail.foldLeft( ps.head.toString )( _ + " OR " + _ )
	}

	def cast[A]( f: A => Boolean ) = new P[A] {
		def apply( a: A ) = f( a )
	}
}

