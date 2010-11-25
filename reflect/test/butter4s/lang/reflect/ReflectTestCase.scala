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
import java.io.Serializable
import java.lang.annotation.RetentionPolicy

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */

class ReflectTestCase {
	@Test def testClassType = {
		val t = typeOf[String]
		println( t )
		assertNotNull( t )
		assertTrue( t.isInstanceOf[ClassType[String]] )
		assertNotNull( t.asInstanceOf[ClassType[String]].newInstance )
	}

	@Test def testInterfaceType = {
		val t = typeOf[Serializable]
		println( t )
		assertNotNull( t )
		assertTrue( t.isInstanceOf[InterfaceType[Serializable]] )
	}

	@Test def testAnnotationType = {
		val t = typeOf[Test]
		println( t )
		assertNotNull( t )
		assertTrue( t.isInstanceOf[AnnotationType[Test]] )
	}

	@Test def testEnumType = {
		val t = typeOf[RetentionPolicy]
		println( t )
		assertNotNull( t )
		assertTrue( t.isInstanceOf[EnumType[RetentionPolicy]] )
		println( t.asInstanceOf[EnumType[RetentionPolicy]].values.toList )
	}
}