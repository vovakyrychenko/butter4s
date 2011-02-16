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

package butter4s.net.http.rest.servlet

import javax.servlet.http.{HttpSession, HttpServletResponse, HttpServletRequest, HttpServlet}
import java.io.Writer
import butter4s.net.http.{HttpMethod, rest}

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */
class ServletService extends HttpServlet with rest.Service {
	override def doDelete(req: HttpServletRequest, resp: HttpServletResponse) = doIt(req, resp)

	override def doPut(req: HttpServletRequest, resp: HttpServletResponse) = doIt(req, resp)

	override def doHead(req: HttpServletRequest, resp: HttpServletResponse) = doIt(req, resp)

	override def doPost(req: HttpServletRequest, resp: HttpServletResponse) = doIt(req, resp)

	override def doGet(req: HttpServletRequest, resp: HttpServletResponse) = doIt(req, resp)

	def doIt(req: HttpServletRequest, resp: HttpServletResponse) = perform(new ServletRequestAdapter(req, this), new ServletResponseAdapter(resp))

}

class ServletRequestAdapter(req: HttpServletRequest, servlet: HttpServlet) extends rest.Request {
	def parameters(name: String) = req.getParameterValues(name).toList

	def parameter(name: String) = Option(req.getParameter(name))

	lazy val requestLine = req.getRequestURI.substring(req.getServletPath.length)

	lazy val httpMethod = HttpMethod.valueOf(req.getMethod.toUpperCase)

	lazy val context = new ServletContextAdapter(req, servlet)

	lazy val session = new ServletSessionAdapter(req.getSession)

	lazy val body = req.getInputStream
}

class ServletContextAdapter(req: HttpServletRequest, servlet: HttpServlet) extends rest.Context {
	lazy val serviceLocation = req.getRequestURI.substring(0, req.getServletPath.length)

	lazy val serviceName = servlet.getServletConfig.getServletName
}

class ServletSessionAdapter(s: HttpSession) extends rest.Session {
	def apply[A](name: String) = Option(s.getAttribute(name).asInstanceOf[A])

	def update(name: String, value: Any) = s.setAttribute(name, value)

	def invalidate = s.invalidate
}

class ServletResponseAdapter(resp: HttpServletResponse) extends rest.Response {
	def status(code: Int, message: String = null) = if( code < 400 ) {
		resp.setContentType("text/plain");
		resp.setStatus(code);
	} else resp.sendError(code, message)

	def content(contentType: String, what: (=> Writer) => Unit) = {
		resp.setContentType(contentType)
		what(resp.getWriter)
	}
}