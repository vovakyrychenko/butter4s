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

import reflect.{NativeParameterized, ManifestParameterized, PrimitiveType, EnumType, AnnotationType, Type, InterfaceType, ClassType}

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */

package object reflect {
	class RichManifest[A]( m: Manifest[A] ) {
		def asType: Type[A] = {
			val javaClass = m.erasure.asInstanceOf[Class[A]]
			if ( javaClass.isAnnotation ) new AnnotationType[A]( javaClass )
			else if ( javaClass.isEnum ) new EnumType[A]( javaClass )
			else if ( javaClass.isInterface ) new InterfaceType[A]( javaClass ) with ManifestParameterized[A] {val manifest = m}
			else if ( javaClass.isPrimitive ) new PrimitiveType[A]( javaClass )
			else new ClassType[A]( javaClass ) with ManifestParameterized[A] {val manifest = m}
		}
	}

	implicit def toRichManifest[A]( m: Manifest[A] ) = new RichManifest[A]( m )

	def typeOf[A: Manifest]: Type[A] = manifest[A].asType

	class RichClass[A]( javaClass: Class[A] ) {
		def asType: Type[A] =
			if ( javaClass.isAnnotation ) new AnnotationType[A]( javaClass )
			else if ( javaClass.isEnum ) new EnumType[A]( javaClass )
			else if ( javaClass.isInterface ) new InterfaceType[A]( javaClass )
			else if ( javaClass.isPrimitive ) new PrimitiveType[A]( javaClass )
			else new ClassType[A]( javaClass )
	}

	implicit def toRichClass[A]( javaClass: Class[A] ) = new RichClass[A]( javaClass )

	class RichJavaType( t: java.lang.reflect.Type ) {
		def asType: Type[_] = {
			if ( t.isInstanceOf[Class[_]] ) t.asInstanceOf[Class[_]].asType
			else if ( t.isInstanceOf[java.lang.reflect.ParameterizedType] ) {
				val ptype = t.asInstanceOf[java.lang.reflect.ParameterizedType]
				val javaClass = ptype.getRawType.asInstanceOf[Class[Any]]
				if ( javaClass.isInterface ) new InterfaceType[Any]( javaClass ) with NativeParameterized[Any] {val javaType = ptype}
				else new ClassType[Any]( javaClass ) with NativeParameterized[Any] {val javaType = ptype}
			} else throw new RuntimeException( "support for " + t + " is not implemented yet" )
		}
	}

	implicit def toRichJavaType( javaType: java.lang.reflect.Type ) = new RichJavaType( javaType )

	trait Typed[A] {
		def typeOf: Type[A]
	}

	implicit def toTypedAnyRef[A <: AnyRef]( a: A ) = new Typed[A] {
		def typeOf = a.getClass.asInstanceOf[Class[A]].asType
	}

	implicit def toTypedAnyVal[A <: AnyVal]( a: A ) = new Typed[A] {
		def typeOf = a match {
			case a: Int => new PrimitiveType[Int]( classOf[Int] ).asInstanceOf[Type[A]]
			case a: Short => new PrimitiveType[Short]( classOf[Short] ).asInstanceOf[Type[A]]
			case a: Long => new PrimitiveType[Long]( classOf[Long] ).asInstanceOf[Type[A]]
			case a: Byte => new PrimitiveType[Byte]( classOf[Byte] ).asInstanceOf[Type[A]]
			case a: Float => new PrimitiveType[Float]( classOf[Float] ).asInstanceOf[Type[A]]
			case a: Double => new PrimitiveType[Double]( classOf[Double] ).asInstanceOf[Type[A]]
			case a: Boolean => new PrimitiveType[Boolean]( classOf[Boolean] ).asInstanceOf[Type[A]]
			case a: Char => new PrimitiveType[Char]( classOf[Char] ).asInstanceOf[Type[A]]
		}
	}
}