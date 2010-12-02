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

import annotation.tailrec
import butter4s.lang.reflect.raw

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */

trait RefType[T] extends Type[T] {
	lazy val fields = {
		@tailrec def find( fields: List[Field[RefType.this.type, T]], clazz: Class[_] ): List[Field[RefType.this.type, T]] =
			if ( clazz == null ) fields else find( fields ::: clazz.getDeclaredFields.map( new Field[RefType.this.type, T]( _, RefType.this ) ).toList, clazz.getSuperclass )

		find( List[Field[RefType.this.type, T]](), javaClass )
	}

	lazy val methods = {
		@tailrec def find( methods: List[Method[RefType.this.type, T]], clazz: Class[_] ): List[Method[RefType.this.type, T]] =
			if ( clazz == null ) methods else find( methods ::: clazz.getDeclaredMethods.map( new Method[RefType.this.type, T]( _, RefType.this ) ).toList, clazz.getSuperclass )

		find( List[Method[RefType.this.type, T]](), javaClass )
	}
}


class Field[DT <: RefType[T], T] private[parameterized]( protected val javaField: java.lang.reflect.Field, val declaringType: DT ) {
	lazy val rawField = new raw.Field[T]( javaField )

	lazy val actualType: Type[_] = javaField.getGenericType match {
		case t: java.lang.reflect.TypeVariable[_] => declaringType.actualTypeOf( t.getName )
		case t: java.lang.reflect.ParameterizedType => Type.fromParameterizedType( t )
		case t: Class[_] => Type.fromClass( t )
		case x => throw new RuntimeException( x.getClass + " not yet supported" )
	}

	override def toString = "field " + rawField.name
}

class Method[DT <: RefType[T], T] private[parameterized]( protected val javaMethod: java.lang.reflect.Method, val declaringType: DT ) {
	lazy val rawMethod = new raw.Method[T]( javaMethod )

	lazy val parameters = javaMethod.getParameterTypes.zip( javaMethod.getGenericParameterTypes ).zip( javaMethod.getParameterAnnotations ).map {case ((cls, t), as) => new Parameter( cls, t, as.toList, Method.this )}

	override def toString = "method " + rawMethod.name
}

class Parameter[DT <: RefType[T], T] private[parameterized]( protected val javaClass: Class[_], protected val nativeType: java.lang.reflect.Type, val annotations: List[java.lang.annotation.Annotation], val declaringMethod: Method[DT, T] ) {
	lazy val rawParameter = new raw.Parameter( javaClass, annotations )

	lazy val actualType: Type[_] = nativeType match {
		case t: java.lang.reflect.TypeVariable[_] => declaringMethod.declaringType.actualTypeOf( t.getName )
		case t: java.lang.reflect.ParameterizedType => Type.fromParameterizedType( t )
		case t: Class[_] => Type.fromClass( t )
		case x => throw new RuntimeException( x.getClass + " not yet supported" )
	}

	override def toString = "parameter " + nativeType
}
