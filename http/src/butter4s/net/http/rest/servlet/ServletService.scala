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

package butter4s.net.http.rest.servlet

import butter4s.net.http.rest
import javax.servlet.http.{HttpSession, HttpServletResponse, HttpServletRequest, HttpServlet}
import java.io.Writer

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */
class ServletService extends HttpServlet with rest.Service {
	override def doPost( req: HttpServletRequest, resp: HttpServletResponse ) = doGet( req, resp )

	override def doGet( req: HttpServletRequest, resp: HttpServletResponse ) = perform( new ServletRequestAdapter( req, this ), new ServletResponseAdapter( resp ) )

}

class ServletRequestAdapter( req: HttpServletRequest, servlet: HttpServlet ) extends rest.Request {
	def parameters( name: String ) = req.getParameterValues( name ).toList

	def parameter( name: String ) = Option( req.getParameter( name ) )

	lazy val requestLine = req.getRequestURI.substring( req.getServletPath.length )

	lazy val context = new ServletContextAdapter( req, servlet )

	lazy val session = new ServletSessionAdapter( req.getSession )

	lazy val body = req.getInputStream
}

class ServletContextAdapter( req: HttpServletRequest, servlet: HttpServlet ) extends rest.Context {
	lazy val serviceLocation = req.getRequestURI.substring( 0, req.getServletPath.length )

	lazy val serviceName = servlet.getServletConfig.getServletName
}

class ServletSessionAdapter( s: HttpSession ) extends rest.Session {
	def apply[A]( name: String ) = Option( s.getAttribute( name ).asInstanceOf[A] )

	def update( name: String, value: Any ) = s.setAttribute( name, value )

	def invalidate = s.invalidate
}

class ServletResponseAdapter( resp: HttpServletResponse ) extends rest.Response {
	def status( code: Int, message: String = null ) = if ( code < 400 ) resp.setStatus( code ) else resp.sendError( code, message )

	def content( contentType: String, what: ( => Writer ) => Unit ) = {
		resp.setContentType( contentType )
		what( resp.getWriter )
	}
}