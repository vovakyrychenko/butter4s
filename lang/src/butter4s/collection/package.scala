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

import collection.mutable.MultiArrayMap

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */
package object collection {
	class RichTraversableOnce[A]( t: TraversableOnce[A] ) {
		import scala.collection.mutable
		def toMultiArrayMap[K, V]( implicit ev: A <:< (K, V) ): MultiArrayMap[K, V] = {
			val r = new mutable.HashMap[K, mutable.ArrayBuffer[V]] with MultiArrayMap[K, V]
			for ( a <- t ) r.addBinding( a )
			r
		}
	}

	implicit def toRichTraversableOnce[A]( t: TraversableOnce[A] ) = new RichTraversableOnce( t )

	class RichArray[A]( ar: Array[A] ) {
		import scala.collection.mutable
		def toMultiArrayMap[K, V]( implicit ev: A <:< (K, V) ): MultiArrayMap[K, V] = {
			val r = new mutable.HashMap[K, mutable.ArrayBuffer[V]] with MultiArrayMap[K, V]
			for ( a <- ar ) r.addBinding( a )
			r
		}
	}

	implicit def toRichArray[A]( t: Array[A] ) = new RichArray( t )
}