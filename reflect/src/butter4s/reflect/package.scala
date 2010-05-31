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

package butter4s

import butter4s.reflect._

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */
package object reflect {
	implicit def convertClass[A](clazz: java.lang.Class[_]): Clazz[A] = new Clazz[A](clazz)

	implicit def unconvertClass[A](clazz: Clazz[A]): Class[A] = clazz.impl.asInstanceOf[Class[A]]

	implicit def convertField(field: java.lang.reflect.Field): Field = new Field(field)

	implicit def unconvertField(field: Field): java.lang.reflect.Field = field.impl

	implicit def convertMethod(method: java.lang.reflect.Method): Method = new Method(method)

	implicit def unconvertMethod(method: Method): java.lang.reflect.Method = method.impl

	implicit def convertType(t: java.lang.reflect.Type): Type = new Type(t)

	implicit def unconvertType(t: Type): java.lang.reflect.Type = t.impl

	def typeOf(value: Any): Class[_] = value match {
		case v: AnyRef => v.getClass
		case c: Int => classOf[Int]
		case _: Long => classOf[Long]
		case _: Byte => classOf[Byte]
		case _: Short => classOf[Short]
		case _: Double => classOf[Double]
		case _: Float => classOf[Float]
		case _: Boolean => classOf[Boolean]
	}
}