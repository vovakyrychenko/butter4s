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