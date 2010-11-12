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
package butter4s.rest

import butter4s.bind.json.JsonBind
import butter4s.reflect._
import java.lang.reflect.Type

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */

object Convert {
	private val converters: Map[String, ( (String, String, Type) => Any )] = Map(
		classOf[Int].getName -> {(s, _, _) => s.toInt},
		classOf[java.lang.Integer].getName -> {(s, _, _) => s.toInt},
		classOf[Long].getName -> {(s, _, _) => s.toLong},
		classOf[java.lang.Long].getName -> {(s, _, _) => s.toLong},
		classOf[Short].getName -> ( (s, _, _) => s.toShort ),
		classOf[java.lang.Short].getName -> ( (s, _, _) => s.toShort ),
		classOf[Byte].getName -> ( (s, _, _) => s.toByte ),
		classOf[java.lang.Byte].getName -> ( (s, _, _) => s.toByte ),
		classOf[Float].getName -> ( (s, _, _) => s.toFloat ),
		classOf[java.lang.Float].getName -> ( (s, _, _) => s.toFloat ),
		classOf[Double].getName -> ( (s, _, _) => s.toDouble ),
		classOf[java.lang.Double].getName -> {(s, _, _) => s.toDouble},
		classOf[Boolean].getName -> {(s, _, _) => s.toBoolean},
		classOf[java.lang.Boolean].getName -> {(s, _, _) => s.toBoolean},
		classOf[String].getName -> {(s, _, _) => s},
		MimeType.APPLICATION_JSON -> {(s, _, t) => JsonBind.unmarshal( s, t ).get}
		)

	def to( value: String, hint: String, targetType: Type ) = ( if ( hint == MimeType.APPLICATION_JAVA_CLASS ) converters( targetType.toClass[AnyRef].getName )
	else converters( hint ) )( value, hint, targetType )
}