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
package butter4s.servlet


/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */
class Request( private[servlet] val impl: javax.servlet.http.HttpServletRequest ) {
	lazy val session = new Session( impl.getSession )

	def apply[A: Manifest]( name: String ): Option[A] =
		if ( manifest[A].erasure.isAssignableFrom( classOf[String] ) ) Option( impl.getParameter( name ).asInstanceOf[A] )
		else if ( manifest[A].erasure.isAssignableFrom( classOf[List[String]] ) ) Option( impl.getParameterValues( name ).toList.asInstanceOf[A] )
		else throw new ClassCastException( "request could provide value of type String or List[String]" )
}