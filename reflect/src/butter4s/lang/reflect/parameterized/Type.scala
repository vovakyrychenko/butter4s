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
package butter4s.lang.reflect.parameterized

import butter4s.lang.reflect.raw
import butter4s.lang._

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */

object Type {
	def fromManifest[ A ](m: Manifest[ A ]): Type[ A ] = asParameterizedType(m.erasure.asInstanceOf[ Class[ A ] ], new ManifestParameterizedType[ A ](m))

	def fromClass[ A ](javaClass: Class[ A ]): Type[ A ] = asParameterizedType(javaClass, new ClassParameterizedType[ A ](javaClass))

	def fromParameterizedType[ A ](pt: java.lang.reflect.ParameterizedType): Type[ A ] = asParameterizedType(pt.getRawType.asInstanceOf[ Class[ A ] ], pt)

	private def asParameterizedType[ A ](javaClass: Class[ A ], pt: java.lang.reflect.ParameterizedType): Type[ A ] = {
		if( javaClass.isAnnotation ) new AnnotationType[ A ](pt)
		else if( javaClass.isEnum ) new EnumType[ A ](pt)
		else if( javaClass.isArray ) new ArrayType[ A ](pt)
		else if( javaClass.isInterface ) new InterfaceType[ A ](pt)
		else if( javaClass.isPrimitive ) new PrimitiveType[ A ](pt)
		else new ClassType[ A ](pt)
	}

	def fromType(t: java.lang.reflect.Type): Type[ _ ] = t match {
		case t: java.lang.reflect.ParameterizedType => fromParameterizedType(t)
		case t: Class[ _ ] => fromClass(t)
	}
}


private class ManifestParameterizedType[ A ](m: Manifest[ A ]) extends java.lang.reflect.ParameterizedType {
	def getOwnerType = m.erasure.getDeclaringClass

	def getRawType = m.erasure

	def getActualTypeArguments = m.typeArguments.map(new ManifestParameterizedType(_)).toArray

	override def toString = getClass.getSimpleName + "(" + m + ")"
}

/**
 * for java compatibility
 */
abstract class TypeManifest[ T ] extends java.lang.reflect.ParameterizedType {
	private val impl = getClass.getGenericSuperclass.asInstanceOf[ java.lang.reflect.ParameterizedType ].getActualTypeArguments()(0)

	def getOwnerType = if( impl.isInstanceOf[ java.lang.reflect.ParameterizedType ] )
		impl.asInstanceOf[ java.lang.reflect.ParameterizedType ].getOwnerType
	else impl.asInstanceOf[ Class[ _ ] ].getDeclaringClass

	def getRawType = if( impl.isInstanceOf[ java.lang.reflect.ParameterizedType ] )
		impl.asInstanceOf[ java.lang.reflect.ParameterizedType ].getRawType
	else impl.asInstanceOf[ Class[ _ ] ]

	def getActualTypeArguments = if( impl.isInstanceOf[ java.lang.reflect.ParameterizedType ] )
		impl.asInstanceOf[ java.lang.reflect.ParameterizedType ].getActualTypeArguments
	else Array[ java.lang.reflect.Type ]()

	lazy val asParameterizedType = Type.fromParameterizedType[ T ](this)
}


private class ClassParameterizedType[ A ](javaClass: Class[ A ]) extends java.lang.reflect.ParameterizedType {
	assume(javaClass.getTypeParameters.length == 0, javaClass + " is not suitable as parameterized.Type since it has type parameters")

	def getOwnerType = javaClass.getDeclaringClass

	def getRawType = javaClass

	def getActualTypeArguments = Array.empty[ java.lang.reflect.Type ]
}

trait Type[ T ] {
	protected val nativeType: java.lang.reflect.ParameterizedType
	protected lazy val javaClass = nativeType.getRawType.asInstanceOf[ Class[ T ] ]
	lazy val arguments: List[ Type[ _ ] ] = nativeType.getActualTypeArguments.map(t => Type.fromType(t)).toList
	type RawTypeType <: raw.Type[ T ]
	lazy val rawType: RawTypeType = raw.Type.fromClass[ T ](javaClass).asInstanceOf[ RawTypeType ]

	def as[ PT[ x ] <: Type[ x ] ] = this.asInstanceOf[ PT[ T ] ]

	/**
	 *  todo deep resolving of type argument
	 */
	def actualTypeOf(parameter: String) = arguments(rawType.parameters.findIndexOf(_.name == parameter))

	override def hashCode = rawType.## * 41 + arguments.##

	override def equals(that: Any) = that.isInstanceOf[ Type[ _ ] ] && rawType == that.asInstanceOf[ Type[ _ ] ].rawType && arguments == that.asInstanceOf[ Type[ _ ] ].arguments

	override def toString = getClass.getSimpleName + "(" + rawType.simpleName + wrapIf(arguments.length > 0)("[", arguments.mkString(","), "]") + ")"

	def toTypeString: String = rawType.simpleName + wrapIf(arguments.length > 0)("[", arguments.map(_.toTypeString).mkString(","), "]")
}


class ClassType[ T ] private[ parameterized ](protected val nativeType: java.lang.reflect.ParameterizedType) extends RefType[ T ] {
	type RawTypeType = raw.ClassType[ T ]

	def newInstance = nativeType.getRawType.asInstanceOf[ Class[ T ] ].newInstance
}

class InterfaceType[ T ] private[ parameterized ](protected val nativeType: java.lang.reflect.ParameterizedType) extends RefType[ T ] {
	type RawTypeType = raw.InterfaceType[ T ]
}

class AnnotationType[ T ] private[ parameterized ](protected val nativeType: java.lang.reflect.ParameterizedType) extends RefType[ T ] {
	type RawTypeType = raw.AnnotationType[ T ]
}

class PrimitiveType[ T ] private[ parameterized ](protected val nativeType: java.lang.reflect.ParameterizedType) extends Type[ T ] {
	type RawTypeType = raw.PrimitiveType[ T ]
}

class ArrayType[ T ] private[ parameterized ](protected val nativeType: java.lang.reflect.ParameterizedType) extends RefType[ T ] {
	type RawTypeType = raw.ArrayType[ T ]
}

class EnumType[ T ] private[ parameterized ](protected val nativeType: java.lang.reflect.ParameterizedType) extends RefType[ T ] {
	type RawTypeType = raw.EnumType[ T ]
}

