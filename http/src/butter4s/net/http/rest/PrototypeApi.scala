/*
 * The MIT License
 *
 *  Copyright (c) 2011 Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 *
 *  Permission is hereby granted, free of charge, to any person obtaining a copy
 *  of this software and associated documentation files (the "Software"), to deal
 *  in the Software without restriction, including without limitation the rights
 *  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 *  copies of the Software, and to permit persons to whom the Software is
 *  furnished to do so, subject to the following conditions:
 *
 *  The above copyright notice and this permission notice shall be included in
 *  all copies or substantial portions of the Software.
 *
 *  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 *  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 *  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 *  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 *  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 *  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 *  THE SOFTWARE.
 */
package butter4s.net.http.rest

import butter4s.net.http.rest.Method.Constants
import butter4s.lang.reflect._
import butter4s.lang._

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */
object PrototypeApi extends ( (Request, parameterized.ClassType[ AnyRef ], Boolean) => String ) {
	def apply(request: Request, service: parameterized.ClassType[ AnyRef ], qualified: Boolean = false) = "/**\n * prototypejs.org AJAX RPC\n */\nvar api_" + request.context.serviceName +
			" = { \n\tsync: {\n" + service.methods.view.filter(m => m.raw.annotatedWith[ Method ] && m.raw.name != "_api").map(
		method => {
			val params = method.parameters.view.filter(_.raw.annotatedWith[ Param ])
			val queryParams = params.filter(_.raw.annotation[ Param ].get.from == Param.From.QUERY)
			val pathParams = params.filter(_.raw.annotation[ Param ].get.from == Param.From.PATH)
			val bodyParam = params.find(_.raw.annotation[ Param ].get.from == Param.From.BODY)
			val restMethod = method.raw.annotation[ Method ].get
			"\t\t" + method.raw.name + ": function (" + params.map(_.raw.annotation[ Param ].get.name).mkString(",") + ") {\n" +
					"\t\t\tvar result, error;\n" +
					"\t\t\tnew Ajax.Request( '" + ( if( qualified ) request.baseUrl else "" ) + request.context.serviceLocation +
					( if( restMethod.path == Method.Constants.DEFAULT ) "/" + method.raw.name else Request.filter(restMethod.path).replaceAll("\\{", "'+").replaceAll("\\}", "+'") ) + "', {\n" +
					"\t\t\t\tparameters: {\n" +
					queryParams.map(p => {
						val restParam = p.raw.annotation[ Param ].get
						"\t\t\t\t\t" + restParam.name + ":" + ( if( p.raw.rawType <:< typeOf[ List[ _ ] ].rawType ) restParam.name + ".collect(function(x){return " +
								wrapIf(restParam.typeHint != MimeType.APPLICATION_JAVA_CLASS)("Object.toJSON(", "x", ")") + ";})"
						else
							wrapIf(restParam.typeHint != MimeType.APPLICATION_JAVA_CLASS)("Object.toJSON(", restParam.name, ")") )
					}).mkString(",\n") + "\n" +
					"\t\t\t\t},\n" +
					"\t\t\t\tmethod: '" + restMethod.httpMethod()(0) + "',\n" +
					"\t\t\t\tevalJSON: " + MimeType.isJson(restMethod.produces) + ",\n" +
					"\t\t\t\tevalJS: false,\n" +
					"\t\t\t\tasynchronous: false,\n" +
					( if( bodyParam.isDefined ) "\t\t\t\tpostBody: " + bodyParam.get.raw.annotation[ Param ].get.name + ",\n" else "" ) +
					"\t\t\t\tonSuccess: function( response ) {\n" +
					( if( MimeType.isJson(restMethod.produces) ) "\t\t\t\t\tresult = response.responseJSON;\n"
					else if( restMethod.produces == Constants.NONE ) "\t\t\t\t\tresult = response.status;\n" else "\t\t\t\t\tresult = response.responseText;\n" ) +
					"\t\t\t\t},\n" +
					"\t\t\t\tonFailure: function( response ) { \n" +
					"\t\t\t\t\terror = response.statusText;\n" +
					"\t\t\t\t}\n" +
					"\t\t\t});\n" +
					"\t\t\tif (error) throw error; else return result;\n" +
					"\t\t}"
		}).mkString(",\n\n") + "\n\t},\n\tasync: {\n" + service.methods.view.filter(m => m.raw.annotatedWith[ Method ] && m.raw.name != "_api").map(
		method => {
			val params = method.parameters.view.filter(_.raw.annotatedWith[ Param ])
			val queryParams = params.filter(_.raw.annotation[ Param ].get.from == Param.From.QUERY)
			val pathParams = params.filter(_.raw.annotation[ Param ].get.from == Param.From.PATH)
			val bodyParam = params.find(_.raw.annotation[ Param ].get.from == Param.From.BODY)
			val restMethod = method.raw.annotation[ Method ].get
			"\t\t" + method.raw.name + ": function (" + ( params.map(_.raw.annotation[ Param ].get.name) :+ "succeed" :+ "failed" ).mkString(",") + ") {\n" +
					"\t\t\tnew Ajax.Request( '" + ( if( qualified ) request.baseUrl else "" ) + request.context.serviceLocation +
					( if( restMethod.path == Method.Constants.DEFAULT ) "/" + method.raw.name else Request.filter(restMethod.path).replaceAll("\\{", "'+").replaceAll("\\}", "+'") ) + "', {\n" +
					"\t\t\t\tparameters: {\n" +
					queryParams.map(p => {
						val restParam = p.raw.annotation[ Param ].get
						"\t\t\t\t\t" + restParam.name + ":" + ( if( p.raw.rawType <:< typeOf[ List[ _ ] ].rawType ) restParam.name + ".collect(function(x){return " +
								wrapIf(restParam.typeHint != MimeType.APPLICATION_JAVA_CLASS)("Object.toJSON(", "x", ")") + ";})"
						else
							wrapIf(restParam.typeHint != MimeType.APPLICATION_JAVA_CLASS)("Object.toJSON(", restParam.name, ")") )
					}).mkString(",\n") + "\n" +
					"\t\t\t\t},\n" +
					"\t\t\t\tmethod: '" + restMethod.httpMethod()(0) + "',\n" +
					"\t\t\t\tevalJSON: " + MimeType.isJson(restMethod.produces) + ",\n" +
					"\t\t\t\tevalJS: false,\n" +
					( if( bodyParam.isDefined ) "\t\t\t\tpostBody: " + bodyParam.get.raw.annotation[ Param ].get.name + ",\n" else "" ) +
					"\t\t\t\tonSuccess: function( response ) {\n" +
					"\t\t\t\t\tif (succeed) succeed(" +
					( if( MimeType.isJson(restMethod.produces) ) "response.responseJSON"
					else if( restMethod.produces == Constants.NONE ) "response.status" else "response.responseText" ) + ");\n" +
					"\t\t\t\t},\n" +
					"\t\t\t\tonFailure: function( response ) { \n" +
					"\t\t\t\t\tif (failed) failed(response.statusText); else alert(response.statusText);\n" +
					"\t\t\t\t}\n" +
					"\t\t\t});\n" +
					"\t\t}"
		}).mkString(",\n\n") + "\n\t}\n}"
}