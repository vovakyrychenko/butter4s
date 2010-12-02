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

import reflect.{raw, parameterized}

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */
package object reflect {
	def typeOf[A: Manifest] = parameterized.Type.fromManifest( manifest[A] )

	trait RichAny[T] {
		def typeOf: raw.Type[T]
	}

	implicit def toRichAnyRef[T <: AnyRef]( a: T ) = new RichAny[T] {
		def typeOf = raw.Type.fromClass( a.getClass.asInstanceOf[Class[T]] )
	}

	implicit def toRichAnyVal[T <: AnyVal]( a: T ) = new RichAny[T] {
		def typeOf = a match {
			case _: Int => raw.Type.fromClass( classOf[Int] ).asInstanceOf[raw.Type[T]]
			case _: Long => raw.Type.fromClass( classOf[Long] ).asInstanceOf[raw.Type[T]]
			case _: Short => raw.Type.fromClass( classOf[Short] ).asInstanceOf[raw.Type[T]]
			case _: Byte => raw.Type.fromClass( classOf[Byte] ).asInstanceOf[raw.Type[T]]
			case _: Double => raw.Type.fromClass( classOf[Double] ).asInstanceOf[raw.Type[T]]
			case _: Float => raw.Type.fromClass( classOf[Float] ).asInstanceOf[raw.Type[T]]
			case _: Boolean => raw.Type.fromClass( classOf[Boolean] ).asInstanceOf[raw.Type[T]]
			case _: Char => raw.Type.fromClass( classOf[Char] ).asInstanceOf[raw.Type[T]]
		}
	}

}