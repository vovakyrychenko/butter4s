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

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */

class Type( val impl: JType ) {
	def resolveWith( actual: JType ) = impl match {
		case t: Class[_] => t
		case t: ParameterizedType => t
		case tv: TypeVariable[_] =>
			assert( actual.isInstanceOf[ParameterizedType], "actual should be ParameterizedType" )
			val pt = actual.asInstanceOf[ParameterizedType]
			val clazz = pt.getRawType.asInstanceOf[Class[_]]
			pt.getActualTypeArguments()( clazz.getTypeParameters.findIndexOf( _.getName == tv.getName ) )
	}

	def toClass[A]: Clazz[A] = impl match {
		case t: Class[_] => new Clazz[A]( t )
		case t: ParameterizedType => new Clazz[A]( t.getRawType.asInstanceOf[Class[_]] )
		case _ => throw new IllegalArgumentException( "could not get class for " + impl )
	}
}

class Clazz[A]( val impl: Class[_] ) extends AnnotationTarget {
	def declaredFields: List[Field] = {
		@tailrec def find( fields: List[Field], clazz: Class[_] ): List[Field] =
			if ( clazz == null ) fields else find( fields ::: clazz.getDeclaredFields.map( new Field( _ ) ).toList, clazz.getSuperclass )

		find( List[Field](), impl )
	}

	def declaredField( name: String ) = declaredFields.find( _.name == name )

	def declaredMethods: List[Method] = {
		@tailrec def find( methods: List[Method], clazz: Class[_] ): List[Method] =
			if ( clazz == null ) methods else find( methods ::: clazz.getDeclaredMethods.map( new Method( _ ) ).toList, clazz.getSuperclass )

		find( List[Method](), impl )
	}

	def declaredMethod( name: String ) = declaredMethods.find( _.name == name )

	def annotatedField[A <: java.lang.annotation.Annotation : Manifest] = declaredFields.find( _.annotatedWith[A] )

	def assignableFrom[C: Manifest] = impl.isAssignableFrom( manifest[C].erasure )

	override def toString = impl.toString + " as Clazz"
}

class Field( val impl: JField ) extends AnnotationTarget {
	impl.setAccessible( true )

	def name = impl.getName
}

class Method( val impl: JMethod ) extends AnnotationTarget {
	impl.setAccessible( true )

	def name = impl.getName
}

trait AnnotationTarget {
	val impl: AnnotatedElement

	def annotatedWith[A <: java.lang.annotation.Annotation : Manifest] = impl.isAnnotationPresent( manifest[A].erasure.asInstanceOf[Class[A]] )

	def annotation[A <: java.lang.annotation.Annotation : Manifest] = Option( impl.getAnnotation( manifest[A].erasure.asInstanceOf[Class[A]] ) )
}

