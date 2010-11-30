/**************************************************************************
 *
 * Copyright (c) Adstream Holdings Pty Ltd
 *
 * This software is the confidential and proprietary information of
 * Adstream Holdings Pty Ltd ("Confidential Information").  You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Adstream Holdings Pty Ltd.
 *
 ************************************************************************
 */

package butter4s.lang.reflect

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */
class Field( javaField: java.lang.reflect.Field ) extends OverrideAccess[Field] {
	def setAccessible( flag: Boolean ) = javaField.setAccessible( flag )

	lazy val name = javaField.getName

	def get( o: AnyRef ) = javaField.get( o )

	def annotatedWith[A <: java.lang.annotation.Annotation : Manifest] = javaField.isAnnotationPresent( manifest[A].erasure.asInstanceOf[Class[java.lang.annotation.Annotation]] )

	lazy val declaredType = javaField.getGenericType.asType

	def actualType( owner: Parameterized[_] ): Type[_] = declaredType match {
		case t: TypeVariable => owner.arguments( owner.parameters.findIndexOf( _.name == t.name ) )
		case x => x
	}
}