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

object ParameterizedType {
	def fromManifest[A]( m: Manifest[A] ): ParameterizedType[A] = {
		val javaClass = m.erasure.asInstanceOf[Class[A]]
		if ( javaClass.isAnnotation ) new AnnotationType[A]( new ManifestParameterizedType[A]( m ) )
		else if ( javaClass.isEnum ) new EnumType[A]( new ManifestParameterizedType[A]( m ) )
		else if ( javaClass.isArray ) new ArrayType[A]( new ManifestParameterizedType[A]( m ) )
		else if ( javaClass.isInterface ) new InterfaceType[A]( new ManifestParameterizedType[A]( m ) )
		else if ( javaClass.isPrimitive ) new PrimitiveType[A]( new ManifestParameterizedType[A]( m ) )
		else new ClassType[A]( new ManifestParameterizedType[A]( m ) )
	}

	def fromTypeManifest[A]( m: TypeManifest[A] ): ParameterizedType[A] = fromClass( m.erasure.asInstanceOf[Class[A]] )

	def fromClass[A]( javaClass: Class[A] ): ParameterizedType[A] = {
		if ( javaClass.isAnnotation ) new AnnotationType[A]( new ClassParameterizedType[A]( javaClass ) )
		else if ( javaClass.isEnum ) new EnumType[A]( new ClassParameterizedType[A]( javaClass ) )
		else if ( javaClass.isArray ) new ArrayType[A]( new ClassParameterizedType[A]( javaClass ) )
		else if ( javaClass.isInterface ) new InterfaceType[A]( new ClassParameterizedType[A]( javaClass ) )
		else if ( javaClass.isPrimitive ) new PrimitiveType[A]( new ClassParameterizedType[A]( javaClass ) )
		else new ClassType[A]( new ClassParameterizedType[A]( javaClass ) )
	}

	def fromParameterizedType[A]( pt: java.lang.reflect.ParameterizedType ): ParameterizedType[A] = {
		val javaClass = pt.getRawType.asInstanceOf[Class[A]]
		if ( javaClass.isAnnotation ) new AnnotationType[A]( pt )
		else if ( javaClass.isEnum ) new EnumType[A]( pt )
		else if ( javaClass.isArray ) new ArrayType[A]( pt )
		else if ( javaClass.isInterface ) new InterfaceType[A]( pt )
		else if ( javaClass.isPrimitive ) new PrimitiveType[A]( pt )
		else new ClassType[A]( pt )
	}

	def fromType( t: java.lang.reflect.Type ): ParameterizedType[_] = t match {
		case t: java.lang.reflect.ParameterizedType => fromParameterizedType( t )
		case t: Class[_] => fromClass( t )
	}
}


class ManifestParameterizedType[A]( m: Manifest[A] ) extends java.lang.reflect.ParameterizedType {
	def getOwnerType = m.erasure.getDeclaringClass

	def getRawType = m.erasure

	def getActualTypeArguments = m.typeArguments.map( new ManifestParameterizedType( _ ) ).toArray

	override def toString = getClass.getSimpleName + "(" + m + ")"
}

/**
 * for java compatibility
 */
abstract class TypeManifest[T] {
	lazy val erasure = getClass.getGenericSuperclass.asInstanceOf[java.lang.reflect.ParameterizedType].getActualTypeArguments()( 0 ).asInstanceOf[Class[_]]

	lazy val typeArguments = if ( erasure.isInstanceOf[java.lang.reflect.ParameterizedType] )
		erasure.asInstanceOf[java.lang.reflect.ParameterizedType].getActualTypeArguments else Array[java.lang.reflect.Type]()
}


class ClassParameterizedType[A]( javaClass: Class[A] ) extends java.lang.reflect.ParameterizedType {
	def getOwnerType = javaClass.getDeclaringClass

	def getRawType = javaClass

	def getActualTypeArguments = Array.empty[java.lang.reflect.Type]
}

trait ParameterizedType[T] {
	val nativeType: java.lang.reflect.ParameterizedType
	private lazy val javaClass = nativeType.getRawType.asInstanceOf[Class[T]]
	lazy val name = javaClass.getName
	lazy val simpleName = javaClass.getSimpleName
	lazy val arguments: List[ParameterizedType[_]] = nativeType.getActualTypeArguments.map( t => ParameterizedType.fromType( t ) ).toList
	lazy val rawType = RawType.fromClass( javaClass )

	def as[PT[x] <: ParameterizedType[x]] = this.asInstanceOf[PT[T]]


	override def hashCode = rawType.## * 41 + arguments.##

	override def equals( that: Any ) = that.isInstanceOf[ParameterizedType[_]] && rawType == that.asInstanceOf[ParameterizedType[_]].rawType && arguments == that.asInstanceOf[ParameterizedType[_]].arguments

	override def toString = getClass.getSimpleName + "(" + simpleName + ( if ( arguments.length > 0 ) "[" + arguments.mkString( "," ) + "]" else "" ) + ")"
}

trait RefType[T] extends ParameterizedType[T] {
	lazy val fields = {
		@tailrec def find( fields: List[Field], clazz: Class[_] ): List[Field] =
			if ( clazz == null ) fields else find( fields ::: clazz.getDeclaredFields.map( new Field( _ ) ).toList, clazz.getSuperclass )

		find( List[Field](), nativeType.getRawType.asInstanceOf[Class[_]] )
	}
}

class ClassType[T] private[reflect]( val nativeType: java.lang.reflect.ParameterizedType ) extends RefType[T] {
	def newInstance = nativeType.getRawType.asInstanceOf[Class[T]].newInstance
}

class InterfaceType[T] private[reflect]( val nativeType: java.lang.reflect.ParameterizedType ) extends RefType[T] {
}

class AnnotationType[T] private[reflect]( val nativeType: java.lang.reflect.ParameterizedType ) extends RefType[T] {
}

class PrimitiveType[T] private[reflect]( val nativeType: java.lang.reflect.ParameterizedType ) extends ParameterizedType[T] {
}

class ArrayType[T] private[reflect]( val nativeType: java.lang.reflect.ParameterizedType ) extends RefType[T] {
}

class EnumType[T] private[reflect]( val nativeType: java.lang.reflect.ParameterizedType ) extends RefType[T] {
	lazy val values = nativeType.getRawType.asInstanceOf[Class[T]].getEnumConstants.toList
}