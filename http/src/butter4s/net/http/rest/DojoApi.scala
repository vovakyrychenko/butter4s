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
object DojoApi extends ( (Request, parameterized.ClassType[ AnyRef ]) => String ) {
	def apply(request: Request, service: parameterized.ClassType[ AnyRef ]) = "/**\n * dojotoolkit.org AJAX RPC\n */\nvar api_" + request.context.serviceName + " = { \n\tsync: {\n" + service.methods.view.filter(m => m.raw.annotatedWith[ Method ] && m.raw.name != "_api").map(
		method => {
			val params = method.parameters.view.filter(_.raw.annotatedWith[ Param ])
			val queryParams = params.filter(_.raw.annotation[ Param ].get.from == Param.From.QUERY)
			val pathParams = params.filter(_.raw.annotation[ Param ].get.from == Param.From.PATH)
			val bodyParam = params.find(_.raw.annotation[ Param ].get.from == Param.From.BODY)
			val restMethod = method.raw.annotation[ Method ].get
			"\t\t" + method.raw.name + ": function (" + params.map(_.raw.annotation[ Param ].get.name).mkString(",") + ") {\n" +
					"\t\t\tvar result, error;\n" +
					"\t\t\tdojo.xhr( '" + restMethod.httpMethod()(0) + "', {\n" +
					"\t\t\t\turl: '" + request.context.serviceLocation +
					( if( restMethod.path == Method.Constants.DEFAULT ) "/" + method.raw.name else Request.filter(restMethod.path).replaceAll("\\{", "'+").replaceAll("\\}", "+'") ) + "',\n" +
					"\t\t\t\tcontent: {\n" +
					queryParams.map(p => {
						val restParam = p.raw.annotation[ Param ].get
						"\t\t\t\t\t" + restParam.name + ":" + ( if( p.raw.rawType <:< typeOf[ List[ _ ] ].rawType ) "dojo.map( " + restParam.name + ", function(x){return " +
								wrapIf(restParam.typeHint != MimeType.APPLICATION_JAVA_CLASS)("JSON.stringify(", "x", ")") + ";})"
						else
							wrapIf(restParam.typeHint != MimeType.APPLICATION_JAVA_CLASS)("JSON.stringify(", restParam.name, ")") )
					}).mkString(",\n") + "\n" +
					"\t\t\t\t},\n" +
					"\t\t\t\thandleAs: " + ( if( MimeType.isJson(restMethod.produces) ) "'json'" else "'text'" ) + ",\n" +
					"\t\t\t\tsync: true,\n" +
					//FIXME: postData or putData?
					( if( bodyParam.isDefined ) "\t\t\t\tpostData: " + bodyParam.get.raw.annotation[ Param ].get.name + ",\n" else "" ) +
					"\t\t\t\tload: function( response ) {\n" +
					"\t\t\t\t\tresult = response;\n" +
					"\t\t\t\t},\n" +
					"\t\t\t\terror: function( response ) { \n" +
					"\t\t\t\t\terror = response.message;\n" +
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
					"\t\t\treturn dojo.xhr( '" + restMethod.httpMethod()(0) + "', {\n" +
					"\t\t\t\tcontent: {\n" +
					queryParams.map(p => {
						val restParam = p.raw.annotation[ Param ].get
						"\t\t\t\t\t" + restParam.name + ":" + ( if( p.raw.rawType <:< typeOf[ List[ _ ] ].rawType ) "dojo.map( " + restParam.name + ", function(x){return " +
								wrapIf(restParam.typeHint != MimeType.APPLICATION_JAVA_CLASS)("JSON.stringify(", "x", ")") + ";})"
						else
							wrapIf(restParam.typeHint != MimeType.APPLICATION_JAVA_CLASS)("JSON.stringify(", restParam.name, ")") )
					}).mkString(",\n") + "\n" +
					"\t\t\t\t},\n" +
					"\t\t\t\turl: '" + request.context.serviceLocation +
					( if( restMethod.path == Method.Constants.DEFAULT ) "/" + method.raw.name else Request.filter(restMethod.path).replaceAll("\\{", "'+").replaceAll("\\}", "+'") ) + "',\n" +
					"\t\t\t\thandleAs: " + ( if( MimeType.isJson(restMethod.produces) ) "'json'" else "'text'" ) + ",\n" +
					//FIXME: postData or putData?
					( if( bodyParam.isDefined ) "\t\t\t\tpostData: " + bodyParam.get.raw.annotation[ Param ].get.name + ",\n" else "" ) +
					"\t\t\t\tload: function( response ) {\n" +
					"\t\t\t\t\tif (succeed) succeed(" +
					( if( MimeType.isJson(restMethod.produces) ) "response"
					else if( restMethod.produces == Constants.NONE ) "response" else "response" ) + ");\n" +
					"\t\t\t\t},\n" +
					"\t\t\t\terror: function( response ) { \n" +
					"\t\t\t\t\tif (failed) failed(response.message); else alert(response.message);\n" +
					"\t\t\t\t}\n" +
					"\t\t\t});\n" +
					"\t\t}"
		}).mkString(",\n\n") + "\n\t}\n}"

}