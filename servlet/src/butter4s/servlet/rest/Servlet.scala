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
import HttpServletResponse._
import java.io.PrintWriter
import butter4s.bind.json.JSONBind
import butter4s.logging.Logging
import java.lang.reflect.InvocationTargetException

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */
@deprecated
class RestServlet extends butter4s.servlet.Servlet {
	override def post( request: butter4s.servlet.Request, response: butter4s.servlet.Response ) = get( request, response )

	override def get( request: butter4s.servlet.Request, response: butter4s.servlet.Response ) = {
		val action = request.getRequestURI.substring( request.getServletPath.length + 1 )
		val methodName = if ( action.contains( "/" ) ) action.substring( 0, action.indexOf( "/" ) ) else action
		getClass.declaredMethod( methodName ) match {
			case Some( method ) => method.annotation[RestAction] match {
				case Some( a ) => try {
					method.invoke( this, Array[AnyRef](
						new Request( request, if ( a.path != Constants.DEFAULT ) a.path else method.name ),
						new Response( response, a.produces + "; charset=" + a.charset )
						): _* )
				} catch {
					case e: Error => response.sendError( e.code, e.message )
				}
				case None => response.sendError( HttpServletResponse.SC_NOT_FOUND, methodName + " is not a @RestAction" )
			}
			case None => response.sendError( HttpServletResponse.SC_NOT_FOUND, methodName + " not found" )
		}
	}
}

class Request( impl: HttpServletRequest, path: String ) extends butter4s.servlet.Request( impl ) {
	lazy val query = impl.getRequestURI.substring( impl.getServletPath.length + 1 + path.length + ( if ( path.endsWith( "/" ) ) 0 else 1 ) )
}

@deprecated
class Response( impl: HttpServletResponse, contentType: String ) extends butter4s.servlet.Response( impl ) {
	override def send( what: ( => PrintWriter ) => Unit ) = {
		impl.setContentType( contentType )
		super.send( what )
	}
}

class Error( val code: Int, val message: String ) extends RuntimeException( message )

trait Servlet extends butter4s.servlet.Servlet with Logging {
	override def post( request: butter4s.servlet.Request, response: butter4s.servlet.Response ) = get( request, response )

	override def get( request: butter4s.servlet.Request, response: butter4s.servlet.Response ) = {
		val action = request.getRequestURI.substring( request.getServletPath.length + 1 )
		val methodName = if ( action.contains( "/" ) ) action.substring( 0, action.indexOf( "/" ) ) else action
		try {
			getClass.declaredMethod( methodName ) match {
				case None => error( SC_NOT_FOUND, methodName + " is not found" )
				case Some( method ) => method.annotation[Method] match {
					case None => error( SC_NOT_FOUND, methodName + " is not exposed" )
					case Some( restMethod ) =>
						val result = method.invoke( this, method.parameters.map( p => {
							log.debug( p )
							if ( p.clazz == classOf[Request] ) new Request( request, if ( restMethod.path != Constants.DEFAULT ) restMethod.path else method.name )
							else p.annotation[Param] match {
								case None => error( SC_INTERNAL_SERVER_ERROR, "method parameter of type " + p.clazz + " is not annotated properly" )
								case Some( restParam ) => request( restParam.name ) match {
									case None => error( SC_BAD_REQUEST, restParam.name + " is required" )
									case Some( value ) => Convert.to( value, p.clazz ).asInstanceOf[AnyRef]
								}
							}
						} ): _* )
						restMethod.produces match {
							case "application/json" =>
								response.setContentType( restMethod.produces + "; charset=" + restMethod.charset )
								response.send( _.println( if ( restMethod.raw ) result else JSONBind.marshal( result ) ) )
							case _ => response.setStatus( if ( result == null ) SC_OK else result.asInstanceOf[Int] )
						}
				}
			}
		} catch {
			case e: Error => response.sendError( e.code, e.message )
			case e: InvocationTargetException if e.getTargetException.isInstanceOf[Error] => response.sendError( e.getTargetException.asInstanceOf[Error].code, e.getTargetException.getMessage )
			case e: InvocationTargetException => response.sendError( SC_INTERNAL_SERVER_ERROR, e.getTargetException.getMessage )
			case e => response.sendError( SC_INTERNAL_SERVER_ERROR, e.getMessage )
		}
	}

	def error( code: Int, reason: String ) = throw new Error( code, reason )
}


