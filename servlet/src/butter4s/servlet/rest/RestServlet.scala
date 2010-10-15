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

import butter4s.servlet._
import butter4s.reflect._
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}
import java.io.PrintWriter
import butter4s.bind.json.JSONBind

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */
class RestServlet extends Servlet {
	override def post( request: Request, response: Response ) = get( request, response )

	override def get( request: Request, response: Response ) = {
		val action = request.getRequestURI.substring( request.getServletPath.length + 1 )
		val methodName = if ( action.contains( "/" ) ) action.substring( 0, action.indexOf( "/" ) ) else action
		getClass.declaredMethod( methodName ) match {
			case Some( method ) => method.annotation[RestAction] match {
				case Some( a ) => try {
					method.invoke( this, Array[AnyRef](
						new RestRequest( request, if ( a.path != RestConstants.DEFAULT ) a.path else method.name ),
						new RestResponse( response, a.produces + "; charset=" + a.charset )
						): _* )
				} catch {
					case e: RestError => response.sendError( e.code, e.message )
				}
				case None => response.sendError( HttpServletResponse.SC_NOT_FOUND, methodName + " is not a @RestAction" )
			}
			case None => response.sendError( HttpServletResponse.SC_NOT_FOUND, methodName + " not found" )
		}
	}
}

class RestRequest( impl: HttpServletRequest, path: String ) extends Request( impl ) {
	lazy val query = impl.getRequestURI.substring( impl.getServletPath.length + 1 + path.length + ( if ( path.endsWith( "/" ) ) 0 else 1 ) )
}

class RestResponse( impl: HttpServletResponse, contentType: String ) extends Response( impl ) {
	override def send( what: ( => PrintWriter ) => Unit ) = {
		impl.setContentType( contentType )
		super.send( what )
	}
}

class RestError( val code: Int, val message: String ) extends RuntimeException( code + " " + message )

trait RestServlet2 extends Servlet {
	override def post( request: Request, response: Response ) = get( request, response )

	override def get( request: Request, response: Response ) = {
		val action = request.getRequestURI.substring( request.getServletPath.length + 1 )
		val methodName = if ( action.contains( "/" ) ) action.substring( 0, action.indexOf( "/" ) ) else action
		getClass.declaredMethod( methodName ) match {
			case None => response.sendError( HttpServletResponse.SC_NOT_FOUND, methodName + " not found" )
			case Some( method ) => method.annotation[RestMethod] match {
				case None => response.sendError( HttpServletResponse.SC_NOT_FOUND, methodName + " not exposed" )
				case Some( a ) => try {
					val result = method.invoke( this, method.parameters.map( p => Convert.to( request( p.annotation[RestParam].get.value ).get, p.clazz ) ) )
					a.produces match {
						case "application/json" =>
							response.setContentType( a.produces + "; charset=" + a.charset )
							response.send( _.println( if ( a.raw ) result else JSONBind.marshal( result ) ) )
						case _ => response.setStatus( if ( result == null ) HttpServletResponse.SC_OK else result.asInstanceOf[Int] )
					}
				} catch {
					case e: RestError => response.sendError( e.code, e.message )
				}
			}
		}
	}

}


