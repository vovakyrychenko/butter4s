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

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */

class Field private[reflect]( javaField: java.lang.reflect.Field ) {
	lazy val name = javaField.getName

	def accessible[A]( code: => A ) = {
		javaField.setAccessible( true )
		code
	}

	def set( obj: AnyRef, v: Any ) = javaField.set( obj, v )

	def get( obj: AnyRef ) = javaField.get( obj )

	def annotatedWith[A <: java.lang.annotation.Annotation : Manifest] = javaField.isAnnotationPresent( manifest[A].erasure.asInstanceOf[Class[java.lang.annotation.Annotation]] )

	def actualType( p: ParameterizedType[_] ): ParameterizedType[_] = javaField.getGenericType match {
		case t: java.lang.reflect.TypeVariable[_] => p.arguments( p.rawType.parameters.findIndexOf( _.name == t.getName ) )
		case t: java.lang.reflect.ParameterizedType => ParameterizedType.fromParameterizedType( t )
		case t: Class[_] => ParameterizedType.fromClass( t )
		case x => throw new RuntimeException( x.getClass + " not yet supported" )
	}
}