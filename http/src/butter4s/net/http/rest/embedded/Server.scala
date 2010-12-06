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
import org.apache.http.params.{CoreConnectionPNames, CoreProtocolPNames, BasicHttpParams}
import org.apache.http.impl.{DefaultHttpServerConnection, DefaultHttpResponseFactory, DefaultConnectionReuseStrategy}
import butter4s.net.http.rest
import org.apache.http.protocol.{HttpRequestHandler, BasicHttpContext, HttpService, HttpRequestHandlerRegistry, ResponseConnControl, BasicHttpProcessor, ResponseDate, ResponseServer, ResponseContent}
import java.net.{SocketTimeoutException, InetAddress, ServerSocket}

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */
class Server( port: Int, local: Boolean ) extends Logging with Runnable {
	private val serverSocket = if ( local ) new ServerSocket( port, 0, InetAddress.getByName( "localhost" ) ) else new ServerSocket( port )
	locally {
		serverSocket.setSoTimeout( 100 )
	}
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

	registry.register( "/htdocs/*", ClasspathWebInfHandler )

	def add( name: String, service: rest.Service ) = registry.register( "/" + name + "/*", new EmbeddedServiceAdapter( name, service ) )


	def run = {
		log.info( "Ready to rock on " + serverSocket.getLocalSocketAddress )
		while ( !Thread.interrupted ) try {
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
		} catch {
			case e: SocketTimeoutException =>
		}
	}

	def stop = {

	}
}

