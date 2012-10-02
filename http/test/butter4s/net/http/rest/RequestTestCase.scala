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
import butter4s.lang.reflect._
import Assert.assertEquals
import butter4s.net.http.HttpMethod

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */
class RequestTestCase {
	@Test def compile = {
		assertEquals( "^/y/(\\d\\d\\d\\d)/([^/]+)/([^/]+)$", Request.compile( "/y/{year:(\\d\\d\\d\\d)}/{month}/{date}" ).toString )
		assertEquals( "^/y/(\\d{4})/([^/]+)/([^/]+)$", Request.compile( "/y/{year:(\\d{4})}/{month}/{date}" ).toString )
	}

	@Test def pathParam = {
		val mapping = "/y/{year:(\\d{4})}/{month}/{date}"
		val path = "/y/2009/April/12"
		assertEquals( Some( "2009" ), Request.pathParam( mapping, path, "year" ) );
		assertEquals( Some( "April" ), Request.pathParam( mapping, path, "month" ) );
		assertEquals( Some( "12" ), Request.pathParam( mapping, path, "date" ) );
	}

	@Test def methodMatches = {
		assertEquals( "items", typeOf[X].as[parameterized.ClassType].methods.find( Request.methodMatches( "/items", HttpMethod.POST, _ ) ).get.raw.name )
		assertEquals( "item", typeOf[X].as[parameterized.ClassType].methods.find( Request.methodMatches( "/items/1", HttpMethod.POST, _ ) ).get.raw.name )
		assertEquals( "items", typeOf[Y].as[parameterized.ClassType].methods.find( Request.methodMatches( "/", HttpMethod.POST, _ ) ).get.raw.name )
		assertEquals( "item", typeOf[Y].as[parameterized.ClassType].methods.find( Request.methodMatches( "/1", HttpMethod.POST, _ ) ).get.raw.name )
		assertEquals( "api", typeOf[Y].as[parameterized.ClassType].methods.find( Request.methodMatches( "/api", HttpMethod.POST, _ ) ).get.raw.name )
	}
}

class X {
	@Method
	def items = List( 1, 2, 3 )

	@Method( path = "/items/{item}" )
	def item( i: Int ) = i
}

class Y {
	@Method( path = "/" )
	def items = List( 1, 2, 3 )

	@Method
	def api( i: Int ) = i

	@Method( path = "/{item:(\\d+)}" )
	def item( i: Int ) = i

}
