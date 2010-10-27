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
import butter4s.bind.json.JSONBind
import butter4s.logging.Logging
import java.lang.reflect.InvocationTargetException

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */


class Request( impl: HttpServletRequest, path: String ) extends butter4s.servlet.Request( impl ) {
	lazy val query = impl.getRequestURI.substring( impl.getServletPath.length + 1 + path.length + ( if ( path.endsWith( "/" ) ) 0 else 1 ) )
}



class Error( val code: Int, val message: String ) extends RuntimeException( message )

trait Servlet extends butter4s.servlet.Servlet with Logging {
	override def post( request: butter4s.servlet.Request, response: butter4s.servlet.Response ) = get( request, response )

	override def get( request: butter4s.servlet.Request, response: butter4s.servlet.Response ) = try {
		val action = request.getRequestURI.substring( request.getServletPath.length + 1 )
		val methodName = if ( action.contains( "/" ) ) action.substring( 0, action.indexOf( "/" ) ) else action
		getClass.declaredMethod( methodName ) match {
			case None => error( SC_NOT_FOUND, methodName + " is not found" )
			case Some( method ) => method.annotation[Method] match {
				case None => error( SC_NOT_FOUND, methodName + " is not exposed" )
				case Some( restMethod ) =>
					val result = method.invoke( this, method.parameters.map( p => {
						log.debug( p )
						if ( p.clazz.isAssignableFrom( classOf[Request] ) ) new Request( request, if ( restMethod.path != Constants.DEFAULT ) restMethod.path else method.name )
						else p.annotation[Param] match {
							case None => error( SC_INTERNAL_SERVER_ERROR, "method parameter of type " + p.clazz + " is not annotated properly" )
							case Some( restParam ) => request( restParam.name ) match {
								case None => error( SC_BAD_REQUEST, restParam.name + " is required" )
								case Some( value ) => Convert.to( value, p.clazz ).asInstanceOf[AnyRef]
							}
						}
					} ): _* )
					restMethod.produces match {
						case MimeType.APPLICATION_JSON =>
							response.setContentType( restMethod.produces + "; charset=" + restMethod.charset )
							response.send( _.println( if ( restMethod.raw ) result else JSONBind.marshal( result ) ) )
						case MimeType.APPLICATION_JSON_WRAPPED_VALUE =>
							response.setContentType( "application/json; charset=" + restMethod.charset )
							response.send( _.println( """{"value":""" + ( if ( result.isInstanceOf[String] ) "\"" + result + "\"" else result ) + "}" ) )
						case MimeType.TEXT_JAVASCRIPT =>
							response.setContentType( restMethod.produces + "; charset=" + restMethod.charset )
							response.send( _.println( result ) )
						case _ => response.setStatus( if ( result == null ) SC_OK else result.asInstanceOf[Int] )
					}
			}
		}
	} catch {
		case e: Error => error( e, e ); response.sendError( e.code, e.message )
		case e: InvocationTargetException if e.getTargetException.isInstanceOf[Error] => error( e, e ); response.sendError( e.getTargetException.asInstanceOf[Error].code, e.getTargetException.getMessage )
		case e: InvocationTargetException => throw e.getTargetException
	}

	@Method( produces = "text/javascript" )
	def api( request: Request ) = "var api_" + getServletConfig.getServletName + " { \n\tsync: {\n" + getClass.declaredMethods.view.filter( m => m.annotatedWith[Method] && m.name != "api" ).map(
		m => {
			val params = m.parameters.view.filter( _.annotatedWith[Param] )
			"\t\t" + m.name + ": function (" + params.map( _.annotation[Param].get.name ).mkString( "," ) + ") {\n" +
					"\t\t\tvar result, error;\n" +
					"\t\t\tnew Ajax.Request( '" + request.getRequestURI.substring( 0, request.getServletPath.length + 1 ) + m.name + "', {\n" +
					"\t\t\t\tparameters: {\n" +
					params.map( p => "\t\t\t\t\t" + p.annotation[Param].get.name + ":" + p.annotation[Param].get.name ).mkString( ",\n" ) + "\n" +
					"\t\t\t\t},\n" +
					"\t\t\t\tevalJSON:" + MimeType.isJson( m.annotation[Method].get.produces ) + ",\n" +
					"\t\t\t\tevalJS: false,\n" +
					"\t\t\t\tasynchronous: false,\n" +
					"\t\t\t\tonSuccess: function( response ) {\n" +
					( if ( MimeType.isJson( m.annotation[Method].get.produces ) )
						if ( m.annotation[Method].get.produces == MimeType.APPLICATION_JSON_WRAPPED_VALUE ) "\t\t\t\t\tresult = response.responseJSON.value;\n"
						else "\t\t\t\t\tresult = response.responseJSON;\n"
					else if (m.annotation[Method].get.produces == Constants.NONE) "\t\t\t\t\tresult=response.status;\n" else "\t\t\t\t\tresult = response.responseText;\n" ) +
					"\t\t\t\t},\n" +
					"\t\t\t\tonFailure: function( response ) { \n" +
					"\t\t\t\t\terror = response.statusText;\n" +
					"\t\t\t\t}\n" +
					"\t\t\t});\n" +
					"\t\t\tif (error) throw error else return result;\n" +
					"\t\t}"
		} ).mkString( ",\n\n" ) + "\n\t}\n}"


	def error( code: Int, reason: String ) = throw new Error( code, reason )
}


