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

import java.lang.reflect.{Field => JField, Method => JMethod, AnnotatedElement}
import butter4s.reflect._
import scala.annotation.tailrec

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */
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

	def annotatedField[A <: java.lang.annotation.Annotation]( implicit m: Manifest[A] ) = declaredFields.find( _.annotatedWith[A] )

	def assignableFrom[C]( implicit m: Manifest[C] ) = impl.isAssignableFrom( m.erasure )

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

	def annotatedWith[A <: java.lang.annotation.Annotation]( implicit m: Manifest[A] ) = {
		println(m.erasure)
		val present = impl.isAnnotationPresent( m.erasure.asInstanceOf[Class[A]] )
		println(present)
		present
	}

	def annotation[A <: java.lang.annotation.Annotation]( implicit m: Manifest[A] ) = Option( impl.getAnnotation( m.erasure.asInstanceOf[Class[A]] ) )
}