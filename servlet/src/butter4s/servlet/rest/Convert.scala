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
package butter4s.servlet.rest

import butter4s.bind.json.JSONBind
import butter4s.reflect._
import java.lang.reflect.{ParameterizedType, Type}

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */

object Convert {
	private val converters: Map[String, ( (AnyRef, String, Type) => Any )] = Map(
		classOf[Int].getName -> {(s, _, _) => s.asInstanceOf[String].toInt},
		classOf[java.lang.Integer].getName -> {(s, _, _) => s.asInstanceOf[String].toInt},
		classOf[Long].getName -> {(s, _, _) => s.asInstanceOf[String].toLong},
		classOf[java.lang.Long].getName -> {(s, _, _) => s.asInstanceOf[String].toLong},
		classOf[Short].getName -> ( (s, _, _) => s.asInstanceOf[String].toShort ),
		classOf[java.lang.Short].getName -> ( (s, _, _) => s.asInstanceOf[String].toShort ),
		classOf[Byte].getName -> ( (s, _, _) => s.asInstanceOf[String].toByte ),
		classOf[java.lang.Byte].getName -> ( (s, _, _) => s.asInstanceOf[String].toByte ),
		classOf[Float].getName -> ( (s, _, _) => s.asInstanceOf[String].toFloat ),
		classOf[java.lang.Float].getName -> ( (s, _, _) => s.asInstanceOf[String].toFloat ),
		classOf[Double].getName -> ( (s, _, _) => s.asInstanceOf[String].toDouble ),
		classOf[java.lang.Double].getName -> {(s, _, _) => s.asInstanceOf[String].toDouble},
		classOf[Boolean].getName -> {(s, _, _) => s.asInstanceOf[String].toBoolean},
		classOf[java.lang.Boolean].getName -> {(s, _, _) => s.asInstanceOf[String].toBoolean},
		classOf[String].getName -> {(s, _, _) => s.asInstanceOf[String]},
		classOf[List[_]].getName -> {(xs, hint, xst) => xs.asInstanceOf[List[String]].map( to( _, hint, xst.asInstanceOf[ParameterizedType].getActualTypeArguments()( 0 ) ) )},
		MimeType.APPLICATION_JSON -> {(s, _, t) => JSONBind.unmarshal( s.asInstanceOf[String], t )}
		)

	def to( value: AnyRef, hint: String, targetType: Type ) =
		( if ( hint == MimeType.APPLICATION_JAVA_CLASS ) converters( targetType.toClass[AnyRef].getName ) else converters( hint ) )( value, hint, targetType )
}