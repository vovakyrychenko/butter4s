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
package butter4s.lang.reflect

import org.junit.{Test, Assert}
import Assert._
import java.lang.annotation.RetentionPolicy

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */

class ReflectTestCase {
	@Test def testParameterizedTypes = {
		val t = typeOf[List[Map[RetentionPolicy, List[Int]]]]
		assertEquals( "ClassType(List[InterfaceType(Map[EnumType(RetentionPolicy),ClassType(List[PrimitiveType(int)])])])", t.toString )
		println( t )
		println( typeOf[Array[Int]] )
	}

	@Test def testEquals = {
		assertTrue( typeOf[List[String]] == typeOf[List[String]] )
		assertTrue( typeOf[List[String]] != typeOf[List[Int]] )
		assertTrue( typeOf[List[String]].rawType == typeOf[List[Int]].rawType )
	}

	@Test def testAssignableFrom = {
		assertTrue( typeOf[AnyRef].rawType <:< typeOf[String].rawType )
		assertTrue( typeOf[String].rawType <:< typeOf[String].rawType )
		assertFalse( typeOf[String].rawType <:< typeOf[AnyRef].rawType )
	}

	@Test def testFields = {
		val ct = typeOf[X[String]].as[ClassType]
		assertEquals( List( "x", "str", "t" ), ct.fields.map( _.name ) )
		val f = ct.fields.find( _.name == "t" ).get
		assertEquals( typeOf[String], f.actualType( ct ) )
	}
}

class X[T <: AnyRef] {
	var x = 1;
	var str = "aaa";
	var t: T = _
}