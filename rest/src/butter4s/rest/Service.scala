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

package butter4s.rest

import butter4s.reflect._
import butter4s.lang._
import butter4s.bind.json.JsonBind
import butter4s.logging.Logging
import java.lang.reflect.{ParameterizedType, InvocationTargetException}
import java.io.PrintWriter

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */
object Request {
	def pathParam( mapping: String, localPath: String, name: String ) =
		"\\{([^\\}]*)\\}".r.findAllIn( mapping ).indexOf( "{" + name + "}" ) match {
			case -1 => None
			case group => mapping.replaceAll( "\\{([^\\}]*)\\}", "([^/]*)" ).r.findFirstMatchIn( localPath ).map( _.group( group + 1 ) )
		}
}

trait Session {
	def apply[A]( name: String ): Option[A]
}

trait Request extends {
	def servicePath: String

	def serviceLocation: String

	lazy val methodName = if ( servicePath.contains( "/" ) ) servicePath.substring( 0, servicePath.indexOf( "/" ) ) else servicePath
	lazy val methodPath = servicePath.substring( methodName.length )

	def parameter( mapping: String, name: String ) = Request.pathParam( mapping, methodPath, name )

	def parameter( name: String ): Option[String]

	def parameters( name: String ): List[String]

	val session: Session

	override def toString = servicePath
}

object Response {
	object Code {
		final val CONTINUE = 100;
		final val SWITCHING_PROTOCOLS = 101;
		final val OK = 200;
		final val CREATED = 201;
		final val ACCEPTED = 202;
		final val NON_AUTHORITATIVE_INFORMATION = 203;
		final val NO_CONTENT = 204;
		final val RESET_CONTENT = 205;
		final val PARTIAL_CONTENT = 206;
		final val MULTIPLE_CHOICES = 300;
		final val MOVED_PERMANENTLY = 301;
		final val MOVED_TEMPORARILY = 302;
		final val FOUND = 302;
		final val SEE_OTHER = 303;
		final val NOT_MODIFIED = 304;
		final val USE_PROXY = 305;
		final val TEMPORARY_REDIRECT = 307;
		final val BAD_REQUEST = 400;
		final val UNAUTHORIZED = 401;
		final val PAYMENT_REQUIRED = 402;
		final val FORBIDDEN = 403;
		final val NOT_FOUND = 404;
		final val METHOD_NOT_ALLOWED = 405;
		final val NOT_ACCEPTABLE = 406;
		final val PROXY_AUTHENTICATION_REQUIRED = 407;
		final val REQUEST_TIMEOUT = 408;
		final val CONFLICT = 409;
		final val GONE = 410;
		final val LENGTH_REQUIRED = 411;
		final val PRECONDITION_FAILED = 412;
		final val REQUEST_ENTITY_TOO_LARGE = 413;
		final val REQUEST_URI_TOO_LONG = 414;
		final val UNSUPPORTED_MEDIA_TYPE = 415;
		final val REQUESTED_RANGE_NOT_SATISFIABLE = 416;
		final val EXPECTATION_FAILED = 417;
		final val INTERNAL_SERVER_ERROR = 500;
		final val NOT_IMPLEMENTED = 501;
		final val BAD_GATEWAY = 502;
		final val SERVICE_UNAVAILABLE = 503;
		final val GATEWAY_TIMEOUT = 504;
		final val HTTP_VERSION_NOT_SUPPORTED = 505;
	}
}

trait Response {
	def content( contentType: String, what: ( => PrintWriter ) => Unit ): Unit

	def status( code: Int, message: String = null ): Unit
}

class ImmediateResponse( val code: Int, val message: String ) extends RuntimeException( message )

trait Service extends Logging {
	import Response.Code._

	def serviceName: String

	def perform( request: Request, response: Response ) = log.time( request.toString, try {
		log.debug( "invoke " + request )
		getClass.declaredMethod( request.methodName ) match {
			case None => respond( NOT_FOUND, request.methodName + " is not found" )
			case Some( method ) => method.annotation[Method] match {
				case None => respond( NOT_FOUND, request.methodName + " is not exposed" )
				case Some( restMethod ) =>
					val result = method.invoke( this, method.parameters.map( p => {
						log.debug( p )
						if ( p.genericType.assignableFrom[Request] ) request
						else if ( p.genericType.assignableFrom[List[_]] ) p.annotation[Param] match {
							case None => respond( INTERNAL_SERVER_ERROR, "method parameter of type " + p.genericType + " is not annotated properly" )
							case Some( restParam ) => request.parameters( restParam.name ).map( Convert.to( _, restParam.typeHint, p.genericType.asInstanceOf[ParameterizedType].getActualTypeArguments()( 0 ) ) )
						} else p.annotation[Param] match {
							case None => respond( INTERNAL_SERVER_ERROR, "method parameter of type " + p.genericType + " is not annotated properly" )
							case Some( restParam ) => Convert.to( ( restParam.from match {
								case Param.From.QUERY => request.parameter( restParam.name )
								case Param.From.PATH => request.parameter( restMethod.path, restParam.name )
							} ) match {
								case None => respond( BAD_REQUEST, restParam.name + " is required" )
								case Some( value ) => value
							}, restParam.typeHint, p.genericType ).asInstanceOf[AnyRef]
						}
					} ): _ * )

					restMethod.produces match {
						case MimeType.APPLICATION_JSON =>
							response.content( restMethod.produces + "; charset=" + restMethod.charset, _.println( if ( restMethod.raw ) result else JsonBind.marshal( result ) ) )
						case MimeType.APPLICATION_JSON_WRAPPED_VALUE =>
							response.content( "application/json; charset=" + restMethod.charset, _.println( """{"value":""" + ( if ( result.isInstanceOf[String] ) "\"" + result + "\"" else result ) + "}" ) )
						case MimeType.TEXT_JAVASCRIPT =>
							response.content( restMethod.produces + "; charset=" + restMethod.charset, _.println( result ) )
						case _ => response.status( if ( result == null ) OK else result.asInstanceOf[Int] )
					}
			}
		}
	} catch {
		case e: ImmediateResponse => log.error( e, e );
		response.status( e.code, e.message )
		case e: InvocationTargetException if e.getTargetException.isInstanceOf[ImmediateResponse] => log.error( e.getTargetException, e.getTargetException );
		response.status( e.getTargetException.asInstanceOf[ImmediateResponse].code, e.getTargetException.getMessage )
		case e: InvocationTargetException => throw e.getTargetException
	} )

	@Method( produces = "text/javascript" )
	def api( request: Request ) = "var api_" + serviceName + " = { \n\tsync: {\n" + getClass.declaredMethods.view.filter( m => m.annotatedWith[Method] && m.name != "api" ).map(
		method => {
			val params = method.parameters.view.filter( _.annotatedWith[Param] )
			val (queryParams, pathParams) = params.partition( _.annotation[Param].get.from == Param.From.QUERY )
			val restMethod = method.annotation[Method].get
			"\t\t" + method.name + ": function (" + params.map( _.annotation[Param].get.name ).mkString( "," ) + ") {\n" +
					"\t\t\tvar result, error;\n" +
					"\t\t\tnew Ajax.Request( '" + request.serviceLocation + "/" + method.name +
					( if ( !pathParams.isEmpty ) restMethod.path.replaceAll( "\\{", "'+" ).replaceAll( "\\}", "+'" ) else "" ) + "', {\n" +
					"\t\t\t\tparameters: {\n" +
					queryParams.map( p => {
						val restParam = p.annotation[Param].get
						"\t\t\t\t\t" + restParam.name + ":" + ( if ( p.genericType.assignableFrom[List[_]] ) restParam.name + ".collect(function(x){return " +
								wrapIf( restParam.typeHint != Constants.APPLICATION_JAVA_CLASS )( "Object.toJSON(", "x", ")" ) + ";})" else
							wrapIf( restParam.typeHint != Constants.APPLICATION_JAVA_CLASS )( "Object.toJSON(", restParam.name, ")" ) )
					} ).mkString( ",\n" ) + "\n" +
					"\t\t\t\t},\n" +
					"\t\t\t\tmethod: '" + restMethod.http + "',\n" +
					"\t\t\t\tevalJSON: " + MimeType.isJson( restMethod.produces ) + ",\n" +
					"\t\t\t\tevalJS: false,\n" +
					"\t\t\t\tasynchronous: false,\n" +
					"\t\t\t\tonSuccess: function( response ) {\n" +
					( if ( MimeType.isJson( restMethod.produces ) )
						if ( restMethod.produces == MimeType.APPLICATION_JSON_WRAPPED_VALUE ) "\t\t\t\t\tresult = response.responseJSON.value;\n"
						else "\t\t\t\t\tresult = response.responseJSON;\n"
					else if ( restMethod.produces == Constants.NONE ) "\t\t\t\t\tresult = response.status;\n" else "\t\t\t\t\tresult = response.responseText;\n" ) +
					"\t\t\t\t},\n" +
					"\t\t\t\tonFailure: function( response ) { \n" +
					"\t\t\t\t\terror = response.statusText;\n" +
					"\t\t\t\t}\n" +
					"\t\t\t});\n" +
					"\t\t\tif (error) throw error; else return result;\n" +
					"\t\t}"
		} ).mkString( ",\n\n" ) + "\n\t},\n\tasync: {\n" + getClass.declaredMethods.view.filter( m => m.annotatedWith[Method] && m.name != "api" ).map(
		method => {
			val params = method.parameters.view.filter( _.annotatedWith[Param] )
			val (queryParams, pathParams) = params.partition( _.annotation[Param].get.from == Param.From.QUERY )
			val restMethod = method.annotation[Method].get
			"\t\t" + method.name + ": function (" + ( params.map( _.annotation[Param].get.name ) :+ "succeed" :+ "failed" ).mkString( "," ) + ") {\n" +
					"\t\t\tnew Ajax.Request( '" + request.serviceLocation + "/" + method.name +
					( if ( !pathParams.isEmpty ) restMethod.path.replaceAll( "\\{", "'+" ).replaceAll( "\\}", "+'" ) else "" ) + "', {\n" +
					"\t\t\t\tparameters: {\n" +
					queryParams.map( p => {
						val restParam = p.annotation[Param].get
						"\t\t\t\t\t" + restParam.name + ":" + ( if ( p.genericType.assignableFrom[List[_]] ) restParam.name + ".collect(function(x){return " +
								wrapIf( restParam.typeHint != Constants.APPLICATION_JAVA_CLASS )( "Object.toJSON(", "x", ")" ) + ";})" else
							wrapIf( restParam.typeHint != Constants.APPLICATION_JAVA_CLASS )( "Object.toJSON(", restParam.name, ")" ) )
					} ).mkString( ",\n" ) + "\n" +
					"\t\t\t\t},\n" +
					"\t\t\t\tmethod: '" + restMethod.http + "',\n" +
					"\t\t\t\tevalJSON: " + MimeType.isJson( restMethod.produces ) + ",\n" +
					"\t\t\t\tevalJS: false,\n" +
					"\t\t\t\tonSuccess: function( response ) {\n" +
					"\t\t\t\t\tif (succeed) succeed(" +
					( if ( MimeType.isJson( restMethod.produces ) )
						if ( restMethod.produces == MimeType.APPLICATION_JSON_WRAPPED_VALUE ) "response.responseJSON.value"
						else "response.responseJSON"
					else if ( restMethod.produces == Constants.NONE ) "response.status" else "response.responseText" ) + ");\n" +
					"\t\t\t\t},\n" +
					"\t\t\t\tonFailure: function( response ) { \n" +
					"\t\t\t\t\tif (failed) failed(response.statusText); else alert(response.statusText);\n" +
					"\t\t\t\t}\n" +
					"\t\t\t});\n" +
					"\t\t}"
		} ).mkString( ",\n\n" ) + "\n\t}\n}"


	def respond( code: Int, reason: String ) = throw new ImmediateResponse( code, reason )

}


