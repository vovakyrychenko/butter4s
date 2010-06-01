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
package butter4s

import javax.servlet.http.{HttpSession, HttpServletResponse, HttpServletRequest}
import javax.servlet.{ServletResponse, ServletRequest, FilterChain}
import butter4s.servlet._

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */
package object servlet {
	implicit def convertHttpRequest( request: HttpServletRequest ) = new Request( request )

	implicit def convertHttpResponse( response: HttpServletResponse ) = new Response( response )

	implicit def convertRequest( request: ServletRequest ) = new Request( request.asInstanceOf[HttpServletRequest] )

	implicit def unconvertRequest( request: Request ) = request.impl

	implicit def convertResponse( response: ServletResponse ) = new Response( response.asInstanceOf[HttpServletResponse] )

	implicit def unconvertResponse( response: Response ) = response.impl

	implicit def convertChain( chain: FilterChain ) = new Chain( chain )

	implicit def convertServletConfig( config: javax.servlet.ServletConfig ) = new ServletConfig( config )

	implicit def convertFilterConfig( config: javax.servlet.FilterConfig ) = new FilterConfig( config )

	implicit def unconvertSession( session: Session ): HttpSession = session.impl
}