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

import butter4s.logging.Logging
import butter4s.lang.concurrent._
import butter4s.net.http.rest
import org.apache.http.params.{CoreConnectionPNames, CoreProtocolPNames, BasicHttpParams}
import org.apache.http.impl.{DefaultHttpServerConnection, DefaultHttpResponseFactory, DefaultConnectionReuseStrategy}
import org.apache.http.protocol.{HttpContext, HttpRequestHandler, BasicHttpContext, HttpService, HttpRequestHandlerRegistry, ResponseConnControl, BasicHttpProcessor, ResponseDate, ResponseServer, ResponseContent}
import java.net.{URLDecoder, ServerSocket}
import butter4s.lang._
import org.apache.http.{HttpEntityEnclosingRequest, HttpResponse, HttpRequest}
import org.apache.http.util.EntityUtils
import collection.mutable
import mutable.ArrayBuffer
import org.apache.http.entity.{ContentProducer, EntityTemplate}
import java.io.{ByteArrayInputStream, OutputStreamWriter, OutputStream, Writer}

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */
class Server( port: Int ) extends Logging with Runnable {
	private val serverSocket = new ServerSocket( port )
	private val httpParams = new BasicHttpParams()
	locally {
		httpParams.setIntParameter( CoreConnectionPNames.SO_TIMEOUT, 5000 )
		httpParams.setIntParameter( CoreConnectionPNames.SOCKET_BUFFER_SIZE, 8 * 1024 )
		httpParams.setBooleanParameter( CoreConnectionPNames.STALE_CONNECTION_CHECK, false )
		httpParams.setBooleanParameter( CoreConnectionPNames.TCP_NODELAY, true )
		httpParams.setParameter( CoreProtocolPNames.ORIGIN_SERVER, "Butter4sREST/1.0" )
	}
	private val registry = new HttpRequestHandlerRegistry();
	private val processor = new BasicHttpProcessor()
	locally {
		processor.addInterceptor( new ResponseDate() )
		processor.addInterceptor( new ResponseServer() )
		processor.addInterceptor( new ResponseContent() )
		processor.addInterceptor( new ResponseConnControl() )
	}
	private val httpService = new HttpService( processor, new DefaultConnectionReuseStrategy(), new DefaultHttpResponseFactory() ) {
		setParams( httpParams );
		setHandlerResolver( registry );
	}

	def add( name: String, service: rest.Service ) = registry.register( "/" + name + "/*", new EmbeddedServiceAdapter( name, service ) )

	def run = {
		log.info( "Ready to rock on " + serverSocket.getLocalPort )
		while ( !Thread.interrupted ) {
			val socket = serverSocket.accept
			val connection = new DefaultHttpServerConnection() {
				bind( socket, httpParams )
			}
			async {
				log.debug( "accepted connection " + socket )
				try
					httpService.handleRequest( connection, new BasicHttpContext() )
				catch {
					case e => log.error( e, e )
				}
				finally {
					connection.shutdown
				}
			}
		}
	}
}

class EmbeddedServiceAdapter( name: String, service: rest.Service ) extends HttpRequestHandler {
	def handle( req: HttpRequest, resp: HttpResponse, ctx: HttpContext ) =
		service.perform( new EmbeddedRequestAdapter( req, new EmbeddedContext( name ) ), new EmbeddedResponseAdapter( resp ) )
}

class EmbeddedContext( val serviceName: String ) extends rest.Context {
	val serviceLocation = "/" + serviceName
}

object EmbeddedRequestAdapter {
	def parseParams( params: String ): mutable.Map[String, ArrayBuffer[String]] =
		if ( params != null && params != "" && params.indexOf( "\n" ) == params.lastIndexOf( "\n" ) && params.contains( "=" ) )
			params.split( "&" ).map( _.span( _ != '=' ).map {case (n, v) => (n, URLDecoder.decode( v.substring( 1 ), "UTF-8" ))} ).toMultiArrayMap
		else mutable.Map[String, ArrayBuffer[String]]()
}

class EmbeddedRequestAdapter( req: HttpRequest, val context: rest.Context ) extends rest.Request {
	import EmbeddedRequestAdapter._

	private[embedded] lazy val params = parseParams( req.getRequestLine.getUri.substringAfter( "?" ) )
	private[embedded] lazy val postParams = ( if ( req.getRequestLine.getMethod.toUpperCase == "POST" && req.isInstanceOf[HttpEntityEnclosingRequest] ) parseParams( EntityUtils.toString( req.asInstanceOf[HttpEntityEnclosingRequest].getEntity ) )
	else mutable.Map[String, ArrayBuffer[String]]() )

	def parameters( name: String ) = params.get( name ) match {
		case None => postParams.get( name ) match {
			case None => Nil
			case Some( array ) => array.toList
		}
		case Some( array ) => array.toList
	}

	def parameter( name: String ) = parameters( name ) match {
		case Nil => None
		case x :: xs => Some( x )
	}

	lazy val session = new EmbeddedSession

	lazy val requestLine = req.getRequestLine.getUri.substringBefore( "?" ).substring( context.serviceLocation.length )

	lazy val body = if ( req.isInstanceOf[HttpEntityEnclosingRequest] ) req.asInstanceOf[HttpEntityEnclosingRequest].getEntity.getContent else new ByteArrayInputStream( Array[Byte]() )
}

class EmbeddedResponseAdapter( resp: HttpResponse ) extends rest.Response {
	def status( code: Int, message: String ) = {
		resp.setStatusCode( code )
		resp.setReasonPhrase( message )
	}

	def content( contentType: String, what: ( => Writer ) => Unit ) = resp.setEntity( new EntityTemplate( new ContentProducer() {
		def writeTo( out: OutputStream ) = {
			val writer = new OutputStreamWriter( out )
			what( writer )
			writer.flush
		}
	} ) {
		setContentType( contentType )
	} )
}

class EmbeddedSession extends rest.Session {
	private var attributes = Map[String, Any]()

	def invalidate = attributes = Map[String, Any]()

	def update( name: String, value: Any ) = attributes += name -> value

	def apply[A]( name: String ) = attributes.get( name ).asInstanceOf[Option[A]]
}