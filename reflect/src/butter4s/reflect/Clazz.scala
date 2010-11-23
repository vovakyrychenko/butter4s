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
package butter4s.reflect

import java.lang.reflect.{Field => JField, Method => JMethod, AnnotatedElement, Type => JType, ParameterizedType, TypeVariable}
import scala.annotation.tailrec
import java.lang.annotation.Annotation
import java.lang.Class

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */

class Type( val impl: JType ) {
	def resolveWith( actual: ParameterizedType ) = {
		assert( impl.isInstanceOf[TypeVariable[_]], " should be type variable" )
		actual.getActualTypeArguments()( actual.toClass[AnyRef].getTypeParameters.findIndexOf( _.getName == impl.asInstanceOf[TypeVariable[_]].getName ) )
	}

	def toClass[A]: Clazz[A] = impl match {
		case t: Class[_] => new Clazz[A]( t )
		case t: ParameterizedType => new Clazz[A]( t.getRawType.asInstanceOf[Class[_]] )
		case _ => throw new IllegalArgumentException( "could not get class for " + impl )
	}

	def assignableFrom[C: Manifest] = toClass.assignableFrom[C]

	def assignableFrom( c: Class[_] ) = toClass.isAssignableFrom( c )

	override def toString = impl.toString
}

private class Cache[A, B]( load: ( A => B ) ) {
	var map = Map[A, B]()

	def apply( a: A ) = map.get( a ) match {
		case Some( b ) => b
		case None => val b = load( a ); map += a -> b; b
	}
}

object Clazz {
	private val fields = new Cache( (c: Class[_]) => {
		@tailrec def find( fields: List[Field], clazz: Class[_] ): List[Field] =
			if ( clazz == null ) fields else find( fields ::: clazz.getDeclaredFields.map( new Field( _ ) ).toList, clazz.getSuperclass )

		find( List[Field](), c )
	} )

	def declaredFields( c: Class[_] ) = fields( c )

	private val methods = new Cache( (c: Class[_]) => {
		@tailrec def find( methods: List[Method], clazz: Class[_] ): List[Method] =
			if ( clazz == null ) methods else find( methods ::: clazz.getDeclaredMethods.map( new Method( _ ) ).toList, clazz.getSuperclass )

		find( List[Method](), c )
	} )

	def declaredMethods( c: Class[_] ) = methods( c )
}
class Clazz[A]( val impl: Class[_] ) extends AnnotationTarget {
	def declaredFields = Clazz.declaredFields( impl )

	def declaredField( name: String ) = declaredFields.find( _.name == name )

	def declaredMethods = Clazz.declaredMethods( impl )

	def declaredMethod( name: String ) = declaredMethods.find( _.name == name )

	def declaredMethod( p: Method => Boolean ) = declaredMethods.find( p )

	def annotatedField[A <: java.lang.annotation.Annotation : Manifest] = declaredFields.find( _.annotatedWith[A] )

	def assignableFrom[C: Manifest] = impl.isAssignableFrom( manifest[C].erasure )

	override def toString = impl.toString + " as Clazz"
}

class Field( val impl: JField ) extends AnnotationTarget {
	impl.setAccessible( true )

	lazy val name = impl.getName

	override def toString = impl.toGenericString
}

class Method( val impl: JMethod ) extends AnnotationTarget {
	impl.setAccessible( true )

	lazy val name = impl.getName

	lazy val parameters = impl.getGenericParameterTypes.zip( impl.getParameterAnnotations ).map {case (t, a) => new Parameter( t, a )}

	override def toString = impl.toGenericString
}

class Parameter( val genericType: java.lang.reflect.Type, val annotations: Array[java.lang.annotation.Annotation] ) extends AnnotatedElement with AnnotationTarget {
	override val impl = this

	def getDeclaredAnnotations = annotations

	def getAnnotations = annotations

	def getAnnotation[T <: Annotation]( a: Class[T] ) = annotations.find( a isInstance _ ) match {
		case None => null.asInstanceOf[T]
		case Some( a ) => a.asInstanceOf[T]
	}

	def isAnnotationPresent( a: Class[_ <: Annotation] ) = annotations.exists( a isInstance _ )


	override def toString = "param:" + genericType + ":" + annotations.toList
}

trait AnnotationTarget {
	protected val impl: AnnotatedElement

	def annotatedWith[A <: java.lang.annotation.Annotation : Manifest] = impl.isAnnotationPresent( manifest[A].erasure.asInstanceOf[Class[A]] )

	def annotation[A <: java.lang.annotation.Annotation : Manifest] = Option( impl.getAnnotation( manifest[A].erasure.asInstanceOf[Class[A]] ) )
}

