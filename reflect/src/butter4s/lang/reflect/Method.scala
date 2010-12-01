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

class Field private[reflect]( protected val javaField: java.lang.reflect.Field ) {
	lazy val name = javaField.getName

	def accessible[A]( code: => A ) = {
		javaField.setAccessible( true )
		code
	}

	def set( obj: AnyRef, v: Any ) = javaField.set( obj, v )

	def get( obj: AnyRef ) = javaField.get( obj )

	def annotatedWith[A <: java.lang.annotation.Annotation : Manifest] = javaField.isAnnotationPresent( manifest[A].erasure.asInstanceOf[Class[A]] )

	def annotation[A <: java.lang.annotation.Annotation : Manifest] = Option( javaField.getAnnotation( manifest[A].erasure.asInstanceOf[Class[A]] ) )

	def actualType( owner: ParameterizedType[_] ): ParameterizedType[_] = javaField.getGenericType match {
		case t: java.lang.reflect.TypeVariable[_] => owner.actualTypeOf( t.getName )
		case t: java.lang.reflect.ParameterizedType => ParameterizedType.fromParameterizedType( t )
		case t: Class[_] => ParameterizedType.fromClass( t )
		case x => throw new RuntimeException( x.getClass + " not yet supported" )
	}
}

class Method private[reflect]( protected val javaMethod: java.lang.reflect.Method ) {
	lazy val name = javaMethod.getName

	def invoke( obj: AnyRef, params: AnyRef* ) = javaMethod.invoke( obj, params.toArray: _* )

	def annotatedWith[A <: java.lang.annotation.Annotation : Manifest] = javaMethod.isAnnotationPresent( manifest[A].erasure.asInstanceOf[Class[A]] )

	def annotation[A <: java.lang.annotation.Annotation : Manifest] = Option( javaMethod.getAnnotation( manifest[A].erasure.asInstanceOf[Class[A]] ) )

	lazy val parameters = javaMethod.getParameterTypes.zip( javaMethod.getGenericParameterTypes ).zip( javaMethod.getParameterAnnotations ).map {case ((cls, t), ants) => new Parameter( cls, t, ants.toList )}
}

class Parameter private[reflect]( protected val javaClass: Class[_], protected val nativeType: java.lang.reflect.Type, val annotations: List[java.lang.annotation.Annotation] ) {
	lazy val rawType = RawType.fromClass( javaClass )

	def annotation[A <: java.lang.annotation.Annotation : Manifest] = annotations.find( manifest[A].erasure isInstance _ ).asInstanceOf[Option[A]]

	def annotatedWith[A <: java.lang.annotation.Annotation : Manifest] = annotations.exists( manifest[A].erasure isInstance _ )

	/**
	 * todo recursive resoving of type argument
	 */
	def actualType( owner: ParameterizedType[_] ): ParameterizedType[_] = nativeType match {
		case t: java.lang.reflect.TypeVariable[_] => owner.actualTypeOf( t.getName )
		case t: java.lang.reflect.ParameterizedType => ParameterizedType.fromParameterizedType( t )
		case t: Class[_] => ParameterizedType.fromClass( t )
		case x => throw new RuntimeException( x.getClass + " not yet supported" )
	}


	override def toString = "parameter " + nativeType
}