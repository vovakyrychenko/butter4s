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

import org.junit.{Assert, Test}
import butter4s.reflect._
import Assert.assertEquals

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */
class RequestTestCase {
	@Test def pathParam = {
		val mapping = "/y/{year}/{month}/{date}"
		val path = "/y/2009/April/12"
		assertEquals( "2009", Request.pathParam( mapping, path, "year" ).get );
		assertEquals( "April", Request.pathParam( mapping, path, "month" ).get );
		assertEquals( "12", Request.pathParam( mapping, path, "date" ).get );
	}

	@Test def methodMatches = {
		assertEquals( "items", classOf[X].declaredMethod( Request.methodMatches( "/items", _ ) ).get.name )
		assertEquals( "item", classOf[X].declaredMethod( Request.methodMatches( "/items/1", _ ) ).get.name )

	}
}

class X {
	@Method
	def items = List( 1, 2, 3 )

	@Method( path = "/items/{item}" )
	def item( i: Int ) = i
}
