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

package butter4s.net.http.rest.embedded

import java.net.InetSocketAddress
import butter4s.logging.Logging
import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}
import butter4s.net.http.rest.Service
import java.io.{PrintWriter, OutputStreamWriter}

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */
class Server( port: Int ) {
	private var impl: HttpServer = _

	def start = {
		impl = HttpServer.create( new InetSocketAddress( port ), 0 )
		impl.createContext( "/", RestContextHandler )
		impl.start
	}

	def add( service: Service ) = RestContextHandler.services = service :: RestContextHandler.services

	def stop = impl.stop( 0 )

}

private object RestContextHandler extends HttpHandler with Logging {
	var services: List[Service] = Nil

	def handle( ex: HttpExchange ) = {
		ex.getRequestURI.getPath match {
			case "/" =>
				var list = services.foldLeft( "" )( (r, s) => r + "/" + s.serviceName + "\n" )
				val resp = list.getBytes
				ex.sendResponseHeaders( 200, resp.length )
				ex.getResponseHeaders.add( "Content-Type", "text/plain; charset=UTF-8" )
				ex.getResponseBody.write( resp )
			case path =>
				val name = path.substring( 1, path.substring( 1 ).indexOf( "/" ) )
				services.find( s => s.serviceName == name ) match {
					case None => ex.sendResponseHeaders( 404, 0 )
				}
		}
	}
}

