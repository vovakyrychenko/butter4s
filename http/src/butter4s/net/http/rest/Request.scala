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
package butter4s.net.http.rest

import butter4s.net.http.HttpMethod
import java.io.InputStream

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */

object Request {
	private val rxParamRx = "\\{([^\\:]+):([^\\)]+\\))\\}".r
	private val simpleParamRx = "/\\{([^\\}]+)\\}".r

	private[rest] def compile( mapping: String ) = ( "^" + simpleParamRx.replaceAllIn( rxParamRx.replaceAllIn( mapping, "$2" ), "/([^/]+)" ) + "$" ).r

	private[rest] def filter( mapping: String ) = rxParamRx.replaceAllIn( mapping, "{$1}" )

	def pathParam( mapping: String, requestLine: String, name: String ) =
		simpleParamRx.findAllIn( filter( mapping ) ).indexOf( "/{" + name + "}" ) match {
			case -1 => None
			case group => compile( mapping ).findFirstMatchIn( requestLine ).map( _.group( group + 1 ) )
		}

	def methodMatches( requestLine: String, httpMethod: HttpMethod, m: butter4s.lang.reflect.parameterized.Method[_, _] ) = m.raw.annotation[Method] match {
		case None => false
		case Some( restMethod ) =>
			if ( !restMethod.httpMethod.contains( httpMethod ) ) false
			else if ( restMethod.path == Method.Constants.DEFAULT ) requestLine == "/" + m.raw.name
			else compile( restMethod.path ).findFirstMatchIn( requestLine ).isDefined
	}
}

trait Request {
	val requestLine: String

	val baseUrl: String

	val httpMethod: HttpMethod

	val context: Context

	def parameter( mapping: String, name: String ) = Request.pathParam( mapping, requestLine, name )

	def parameter( name: String ): Option[String]

	def parameters( name: String ): List[String]

	val body: InputStream

	val session: Session

	override def toString = requestLine
}
