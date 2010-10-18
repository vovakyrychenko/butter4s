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

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */

object Convert {
	private val converters: Map[Class[_], ( String => Any )] = Map(
		classOf[Int] -> {_.toInt},
		classOf[java.lang.Integer] -> {_.toInt},
		classOf[Long] -> {_.toLong},
		classOf[java.lang.Long] -> {_.toLong},
		classOf[Short] -> ( _.toShort ),
		classOf[java.lang.Short] -> ( _.toShort ),
		classOf[Byte] -> ( _.toByte ),
		classOf[java.lang.Byte] -> ( _.toByte ),
		classOf[Float] -> ( _.toFloat ),
		classOf[java.lang.Float] -> ( _.toFloat ),
		classOf[Double] -> ( _.toDouble ),
		classOf[java.lang.Double] -> {_.toDouble},
		classOf[Boolean] -> {_.toBoolean},
		classOf[java.lang.Boolean] -> {_.toBoolean},
		classOf[String] -> {s => s}
		)

	def to( value: String, clazz: Class[_] ) = converters( clazz )( value )
}