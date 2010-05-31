/**************************************************************************
 *
 * Copyright (c) Adstream Pty Ltd
 *
 * This software is the confidential and proprietary information of
 * Adstream Pty Ltd ("Confidential Information").  You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Adstream Pty Ltd.
 *
 ************************************************************************
 */

package butter4s.reflect

import java.lang.reflect.{Field => JField, Method => JMethod, AnnotatedElement, Type => JType, ParameterizedType, TypeVariable}
import butter4s.reflect._
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

	def assignableFrom[C: Manifest] = impl.isAssignableFrom( implicitly[Manifest[C]].erasure )

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

	def annotatedWith[A <: java.lang.annotation.Annotation : Manifest] = impl.isAnnotationPresent( implicitly[Manifest[A]].erasure.asInstanceOf[Class[A]] )

	def annotation[A <: java.lang.annotation.Annotation : Manifest] = Option( impl.getAnnotation( implicitly[Manifest[A]].erasure.asInstanceOf[Class[A]] ) )
}

