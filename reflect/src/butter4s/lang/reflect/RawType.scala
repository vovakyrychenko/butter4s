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
package butter4s.lang.reflect

import annotation.tailrec

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */
object RawType {
	def fromClass[A]( javaClass: Class[A] ) =
		if ( javaClass.isAnnotation ) new RawAnnotationType[A]( javaClass )
		else if ( javaClass.isEnum ) new RawEnumType[A]( javaClass )
		else if ( javaClass.isArray ) new RawArrayType[A]( javaClass )
		else if ( javaClass.isInterface ) new RawInterfaceType[A]( javaClass )
		else if ( javaClass.isPrimitive ) new RawPrimitiveType[A]( javaClass )
		else new RawClassType[A]( javaClass )
}

trait RawType[T] {
	val javaClass: Class[T]

	lazy val parameters = javaClass.getTypeParameters.map( new TypeVariable( _ ) ).toList

	def as[RT[x] <: RawType[x]] = this.asInstanceOf[RT[T]]

	def <:<( that: RawType[_] ) = javaClass.isAssignableFrom( that.javaClass )

	override def hashCode = javaClass.##

	override def equals( that: Any ) = that.isInstanceOf[RawType[_]] && javaClass == that.asInstanceOf[RawType[_]].javaClass
}

trait RawRefType[T] extends RawType[T] {
	lazy val fields = {
		@tailrec def find( fields: List[Field], clazz: Class[_] ): List[Field] =
			if ( clazz == null ) fields else find( fields ::: clazz.getDeclaredFields.map( new Field( _ ) ).toList, clazz.getSuperclass )

		find( List[Field](), javaClass )
	}
}

class RawClassType[T] private[reflect]( val javaClass: Class[T] ) extends RawRefType[T]
class RawInterfaceType[T] private[reflect]( val javaClass: Class[T] ) extends RawRefType[T]
class RawAnnotationType[T] private[reflect]( val javaClass: Class[T] ) extends RawRefType[T]
class RawEnumType[T] private[reflect]( val javaClass: Class[T] ) extends RawRefType[T]
class RawArrayType[T] private[reflect]( val javaClass: Class[T] ) extends RawRefType[T]
class RawPrimitiveType[T] private[reflect]( val javaClass: Class[T] ) extends RawType[T]
