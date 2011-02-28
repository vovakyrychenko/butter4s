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
package butter4s.net.http.rest.embedded

import org.apache.http.protocol.{HttpContext, HttpRequestHandler}
import butter4s.lang._
import org.apache.http.{HttpEntityEnclosingRequest, HttpResponse, HttpRequest}
import org.apache.http.util.EntityUtils
import collection.mutable
import mutable.ArrayBuffer
import butter4s.net.http.{HttpMethod, rest}
import java.net.URLDecoder
import org.apache.http.entity.{ContentProducer, EntityTemplate}
import java.io.{ByteArrayInputStream, OutputStreamWriter, OutputStream, Writer}

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */
class EmbeddedServiceAdapter(name: String, location: String, service: rest.Service) extends HttpRequestHandler {
	def handle(req: HttpRequest, resp: HttpResponse, ctx: HttpContext) =
		service.perform(new EmbeddedRequestAdapter(req, new EmbeddedContext(name, location)), new EmbeddedResponseAdapter(resp))
}

class EmbeddedContext(val serviceName: String, val serviceLocation: String) extends rest.Context

object EmbeddedRequestAdapter {
	def params(req: HttpRequest) = if( req.getFirstHeader("Content-Type") != null && req.getFirstHeader("Content-Type").getValue.startsWith("application/x-www-form-urlencoded") && req.isInstanceOf[ HttpEntityEnclosingRequest ] )
		parse(req.getRequestLine.getUri.substringAfter("?")) ++ parse(EntityUtils.toString(req.asInstanceOf[ HttpEntityEnclosingRequest ].getEntity))
	else parse(req.getRequestLine.getUri.substringAfter("?"))

	def parse(params: String): mutable.Map[ String, ArrayBuffer[ String ] ] =
		if( params != null && params != "" )
			params.split("&").map(_.span(_ != '=').map {
				case (n, v) => (n, URLDecoder.decode(v.substring(1), "UTF-8"))
			}).toMultiArrayMap
		else mutable.Map[ String, ArrayBuffer[ String ] ]()
}

class EmbeddedRequestAdapter(req: HttpRequest, val context: rest.Context) extends rest.Request {
	private[ embedded ] lazy val params = EmbeddedRequestAdapter.params(req)

	def parameters(name: String) = params.get(name) match {
		case None => Nil
		case Some(array) => array.toList
	}

	def parameter(name: String) = params.get(name).map(b => if( b.size > 0 ) b(0) else null)

	lazy val requestLine = req.getRequestLine.getUri.substringBefore("?").substring(context.serviceLocation.length)

	lazy val httpMethod = HttpMethod.valueOf(req.getRequestLine.getMethod.toUpperCase)

	lazy val session = new EmbeddedSession

	lazy val body = if( req.isInstanceOf[ HttpEntityEnclosingRequest ] ) req.asInstanceOf[ HttpEntityEnclosingRequest ].getEntity.getContent else new ByteArrayInputStream(Array[ Byte ]())
}

class EmbeddedResponseAdapter(resp: HttpResponse) extends rest.Response {
	def status(code: Int, message: String) = {
		resp.setStatusCode(code)
		resp.setReasonPhrase(message)
	}

	def content(ct: String, what: ( => Writer ) => Unit) = resp.setEntity(new EntityTemplate(new ContentProducer() {
		def writeTo(out: OutputStream) = {
			val writer = new OutputStreamWriter(out)
			what(writer)
			writer.flush
		}
	}) {
		setContentType(ct)
	})
}

class EmbeddedSession extends rest.Session {
	private var attributes = Map[ String, Any ]()

	def invalidate = attributes = Map[ String, Any ]()

	def update(name: String, value: Any) = attributes += name -> value

	def apply[ A ](name: String) = attributes.get(name).asInstanceOf[ Option[ A ] ]
}