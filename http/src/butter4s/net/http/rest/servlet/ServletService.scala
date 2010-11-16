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

import java.io.PrintWriter
import butter4s.net.http.rest.{Session, Response, Request, Service}
import javax.servlet.http.{HttpSession, HttpServletResponse, HttpServletRequest, HttpServlet}

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */
class ServletService extends HttpServlet with Service {
	override def doPost( req: HttpServletRequest, resp: HttpServletResponse ) = doGet( req, resp )

	override def doGet( req: HttpServletRequest, resp: HttpServletResponse ) = perform( new ServletRequestAdapter( req ), new ServletResponseAdapter( resp ) )

	def serviceName = getServletConfig.getServletName
}

class ServletRequestAdapter( req: HttpServletRequest ) extends Request {
	def parameters( name: String ) = req.getParameterValues( name ).toList

	def parameter( name: String ) = Option( req.getParameter( name ) )

	def servicePath = req.getRequestURI.substring( req.getServletPath.length + 1 )

	def serviceLocation = req.getRequestURI.substring( 0, req.getServletPath.length + 1 )

	lazy val session = new ServletSessionAdapter( req.getSession )
}

class ServletSessionAdapter( s: HttpSession ) extends Session {
	def apply[A]( name: String ) = Option( s.getAttribute( name ).asInstanceOf[A] )

	def update( name: String, value: Any ) = s.setAttribute( name, value )

	def invalidate = s.invalidate
}

class ServletResponseAdapter( resp: HttpServletResponse ) extends Response {
	def status( code: Int, message: String = null ) = if ( code < 400 ) resp.setStatus( code ) else resp.sendError( code, message )

	def content( contentType: String, what: ( => PrintWriter ) => Unit ) = {
		resp.setContentType( contentType )
		what( resp.getWriter )
	}
}