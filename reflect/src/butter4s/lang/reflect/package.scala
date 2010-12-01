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

import reflect.{RawType, ParameterizedType}

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */
package object reflect {
	def typeOf[A: Manifest] = ParameterizedType.fromManifest( manifest[A] )

	trait RichAny[T] {
		def typeOf: RawType[T]
	}

	implicit def toRichAnyRef[T]( a: AnyRef ) = new RichAny[T] {
		def typeOf = RawType.fromClass( a.getClass.asInstanceOf[Class[T]] )
	}

	implicit def toRichAnyVal[T]( a: AnyVal ) = new RichAny[T] {
		def typeOf = a match {
			case _: Int => RawType.fromClass( classOf[Int] ).asInstanceOf[RawType[T]]
			case _: Long => RawType.fromClass( classOf[Long] ).asInstanceOf[RawType[T]]
			case _: Short => RawType.fromClass( classOf[Short] ).asInstanceOf[RawType[T]]
			case _: Byte => RawType.fromClass( classOf[Byte] ).asInstanceOf[RawType[T]]
			case _: Double => RawType.fromClass( classOf[Double] ).asInstanceOf[RawType[T]]
			case _: Float => RawType.fromClass( classOf[Float] ).asInstanceOf[RawType[T]]
			case _: Boolean => RawType.fromClass( classOf[Boolean] ).asInstanceOf[RawType[T]]
			case _: Char => RawType.fromClass( classOf[Char] ).asInstanceOf[RawType[T]]
		}
	}

}