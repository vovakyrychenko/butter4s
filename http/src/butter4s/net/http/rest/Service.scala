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

import butter4s.lang.reflect._
import butter4s.lang._
import butter4s.io._
import butter4s.json.Binder
import butter4s.logging.Logging
import java.lang.reflect.InvocationTargetException
import java.io.{InputStream, Writer}
import butter4s.net.http.rest.Method.Constants
import butter4s.net.http.HttpMethod

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */
trait Session {
	def apply[A]( name: String ): Option[A]

	def update( name: String, value: Any ): Unit

	def invalidate: Unit
}

trait Context {
	val serviceName: String

	val serviceLocation: String
}

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

	def methodMatches( requestLine: String, httpMethod: HttpMethod, m: butter4s.lang.reflect.parameterized.Method[_, _] ) = m.rawMethod.annotation[Method] match {
		case None => false
		case Some( restMethod ) =>
			if ( !restMethod.httpMethod.contains( httpMethod ) ) false
			else if ( restMethod.path == Method.Constants.DEFAULT ) requestLine == "/" + m.rawMethod.name
			else compile( restMethod.path ).findFirstMatchIn( requestLine ).isDefined
	}
}

trait Request {
	val requestLine: String

	val httpMethod: HttpMethod

	val context: Context

	def parameter( mapping: String, name: String ) = Request.pathParam( mapping, requestLine, name )

	def parameter( name: String ): Option[String]

	def parameters( name: String ): List[String]

	val body: InputStream

	val session: Session

	override def toString = requestLine
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
	def content( contentType: String, what: ( => Writer ) => Unit ): Unit

	def status( code: Int, message: String = null ): Unit
}

class ImmediateResponse( val code: Int, val message: String ) extends RuntimeException( message )

trait ContentProducer {
	def marshal( content: Any ): String
}

trait ParameterConvertor {
	def convert( value: String, t: parameterized.Type[_] )
}

object Service {
	private[rest] var producers = Map[String, Any => String](
		MimeType.TEXT_JAVASCRIPT -> ( content => String.valueOf( content ) ),
		MimeType.APPLICATION_JSON -> ( content => Binder.marshal( content ) )
		)

	def registerContentProducer( contentType: String, cp: ContentProducer ): Unit = registerContentProducer( contentType, cp.marshal( _ ) )

	def registerContentProducer( contentType: String, produce: Any => String ) = producers += contentType -> produce

	private var converters = Map[String, (String, parameterized.Type[_]) => Any](
		typeOf[Int].rawType.name -> {(s, _) => s.toInt},
		typeOf[java.lang.Integer].rawType.name -> {(s, _) => s.toInt},
		typeOf[Long].rawType.name -> {(s, _) => s.toLong},
		typeOf[java.lang.Long].rawType.name -> {(s, _) => s.toLong},
		typeOf[Short].rawType.name -> ( (s, _) => s.toShort ),
		typeOf[java.lang.Short].rawType.name -> ( (s, _) => s.toShort ),
		typeOf[Byte].rawType.name -> ( (s, _) => s.toByte ),
		typeOf[java.lang.Byte].rawType.name -> ( (s, _) => s.toByte ),
		typeOf[Float].rawType.name -> ( (s, _) => s.toFloat ),
		typeOf[java.lang.Float].rawType.name -> ( (s, _) => s.toFloat ),
		typeOf[Double].rawType.name -> ( (s, _) => s.toDouble ),
		typeOf[java.lang.Double].rawType.name -> {(s, _) => s.toDouble},
		typeOf[Boolean].rawType.name -> {(s, _) => s.toBoolean},
		typeOf[java.lang.Boolean].rawType.name -> {(s, _) => s.toBoolean},
		typeOf[String].rawType.name -> {(s, _) => s},
		MimeType.APPLICATION_JSON -> {(s, t) => Binder.unmarshal( s, t ).get}
		)

	def convert( value: String, hint: String, targetType: parameterized.Type[_] ) =
		( if ( hint == MimeType.APPLICATION_JAVA_CLASS ) converters( targetType.rawType.name )
		else converters( hint ) )( value, targetType )

	def registerParameterBinder( typeHint: String, pc: ParameterConvertor ): Unit = registerParameterBinder( typeHint, pc.convert( _, _ ) )

	def registerParameterBinder( typeHint: String, convert: (String, parameterized.Type[_]) => Any ) = converters += typeHint -> convert

}

trait Service extends Logging {
	import Response.Code._

	def perform( request: Request, response: Response ) = log.time( request.toString, try {
		log.debug( "invoke " + request )
		parameterized.Type.fromClass( getClass.asInstanceOf[Class[Service]] ).as[parameterized.ClassType].methods.find( Request.methodMatches( request.requestLine, request.httpMethod, _ ) ) match {
			case None => respond( NOT_FOUND, request.requestLine + " is unbound" )
			case Some( method ) => method.rawMethod.annotation[Method] match {
				case None => respond( NOT_FOUND, method.rawMethod.name + " is not exposed" )
				case Some( restMethod ) =>
					val result = method.rawMethod.invoke( this, method.parameters.map( p => {
						log.debug( p )
						if ( p.rawParameter.rawType <:< typeOf[Request].rawType ) request
						else if ( p.rawParameter.rawType <:< typeOf[List[_]].rawType ) p.rawParameter.annotation[Param] match {
							case None => respond( INTERNAL_SERVER_ERROR, "method parameter of type " + p.rawParameter.rawType + " is not annotated properly" )
							case Some( restParam ) => request.parameters( restParam.name ).map( Service.convert( _, restParam.typeHint, p.actualType.arguments( 0 ) ) )
						} else p.rawParameter.annotation[Param] match {
							case None => respond( INTERNAL_SERVER_ERROR, "method parameter of type " + p.rawParameter.rawType + " is not annotated properly" )
							case Some( restParam ) => Service.convert( ( restParam.from match {
								case Param.From.BODY => Some( request.body.readAs[String] )
								case Param.From.QUERY => request.parameter( restParam.name )
								case Param.From.PATH => request.parameter( restMethod.path, restParam.name )
							} ) match {
								case None => respond( BAD_REQUEST, restParam.name + " is required" )
								case Some( value ) => value
							}, restParam.typeHint, p.actualType ).asInstanceOf[AnyRef]
						}
					} ): _ * )

					if ( restMethod.raw ) response.content( restMethod.produces + "; charset=" + restMethod.charset, _.write( String.valueOf( result ) ) )
					else Service.producers.get( restMethod.produces ) match {
						case None => response.status( if ( result == null ) OK else result.asInstanceOf[Int] )
						case Some( toContent ) => response.content( restMethod.produces + "; charset=" + restMethod.charset, _.write( toContent( result ) ) )
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
	def api( request: Request ) = "var api_" + request.context.serviceName + " = { \n\tsync: {\n" + parameterized.Type.fromClass( getClass ).as[parameterized.ClassType].methods.view.filter( m => m.rawMethod.annotatedWith[Method] && m.rawMethod.name != "api" ).map(
		method => {
			val params = method.parameters.view.filter( _.rawParameter.annotatedWith[Param] )
			val (queryParams, pathParams) = params.partition( _.rawParameter.annotation[Param].get.from == Param.From.QUERY )
			val restMethod = method.rawMethod.annotation[Method].get
			"\t\t" + method.rawMethod.name + ": function (" + params.map( _.rawParameter.annotation[Param].get.name ).mkString( "," ) + ") {\n" +
					"\t\t\tvar result, error;\n" +
					"\t\t\tnew Ajax.Request( '" + request.context.serviceLocation +
					( if ( restMethod.path == Method.Constants.DEFAULT ) "/" + method.rawMethod.name else Request.filter( restMethod.path ).replaceAll( "\\{", "'+" ).replaceAll( "\\}", "+'" ) ) + "', {\n" +
					"\t\t\t\tparameters: {\n" +
					queryParams.map( p => {
						val restParam = p.rawParameter.annotation[Param].get
						"\t\t\t\t\t" + restParam.name + ":" + ( if ( p.rawParameter.rawType <:< typeOf[List[_]].rawType ) restParam.name + ".collect(function(x){return " +
								wrapIf( restParam.typeHint != MimeType.APPLICATION_JAVA_CLASS )( "Object.toJSON(", "x", ")" ) + ";})" else
							wrapIf( restParam.typeHint != MimeType.APPLICATION_JAVA_CLASS )( "Object.toJSON(", restParam.name, ")" ) )
					} ).mkString( ",\n" ) + "\n" +
					"\t\t\t\t},\n" +
					"\t\t\t\tmethod: '" + restMethod.httpMethod()( 0 ) + "',\n" +
					"\t\t\t\tevalJSON: " + MimeType.isJson( restMethod.produces ) + ",\n" +
					"\t\t\t\tevalJS: false,\n" +
					"\t\t\t\tasynchronous: false,\n" +
					"\t\t\t\tonSuccess: function( response ) {\n" +
					( if ( MimeType.isJson( restMethod.produces ) ) "\t\t\t\t\tresult = response.responseJSON;\n"
					else if ( restMethod.produces == Constants.NONE ) "\t\t\t\t\tresult = response.status;\n" else "\t\t\t\t\tresult = response.responseText;\n" ) +
					"\t\t\t\t},\n" +
					"\t\t\t\tonFailure: function( response ) { \n" +
					"\t\t\t\t\terror = response.statusText;\n" +
					"\t\t\t\t}\n" +
					"\t\t\t});\n" +
					"\t\t\tif (error) throw error; else return result;\n" +
					"\t\t}"
		} ).mkString( ",\n\n" ) + "\n\t},\n\tasync: {\n" + parameterized.Type.fromClass( getClass ).as[parameterized.ClassType].methods.view.filter( m => m.rawMethod.annotatedWith[Method] && m.rawMethod.name != "api" ).map(
		method => {
			val params = method.parameters.view.filter( _.rawParameter.annotatedWith[Param] )
			val (queryParams, pathParams) = params.partition( _.rawParameter.annotation[Param].get.from == Param.From.QUERY )
			val restMethod = method.rawMethod.annotation[Method].get
			"\t\t" + method.rawMethod.name + ": function (" + ( params.map( _.rawParameter.annotation[Param].get.name ) :+ "succeed" :+ "failed" ).mkString( "," ) + ") {\n" +
					"\t\t\tnew Ajax.Request( '" + request.context.serviceLocation +
					( if ( restMethod.path == Method.Constants.DEFAULT ) "/" + method.rawMethod.name else Request.filter( restMethod.path ).replaceAll( "\\{", "'+" ).replaceAll( "\\}", "+'" ) ) + "', {\n" +
					"\t\t\t\tparameters: {\n" +
					queryParams.map( p => {
						val restParam = p.rawParameter.annotation[Param].get
						"\t\t\t\t\t" + restParam.name + ":" + ( if ( p.rawParameter.rawType <:< typeOf[List[_]].rawType ) restParam.name + ".collect(function(x){return " +
								wrapIf( restParam.typeHint != MimeType.APPLICATION_JAVA_CLASS )( "Object.toJSON(", "x", ")" ) + ";})" else
							wrapIf( restParam.typeHint != MimeType.APPLICATION_JAVA_CLASS )( "Object.toJSON(", restParam.name, ")" ) )
					} ).mkString( ",\n" ) + "\n" +
					"\t\t\t\t},\n" +
					"\t\t\t\tmethod: '" + restMethod.httpMethod()( 0 ) + "',\n" +
					"\t\t\t\tevalJSON: " + MimeType.isJson( restMethod.produces ) + ",\n" +
					"\t\t\t\tevalJS: false,\n" +
					"\t\t\t\tonSuccess: function( response ) {\n" +
					"\t\t\t\t\tif (succeed) succeed(" +
					( if ( MimeType.isJson( restMethod.produces ) ) "response.responseJSON"
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


