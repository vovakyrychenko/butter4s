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
package butter4s.persistent

import org.junit.Assert._
import butter4s.json.annotation.Attribute
import annotation.target.field
import butter4s.fs.Directory
import org.junit.{Before, Test}
import java.lang.String
import org.scalatest.junit.{ShouldMatchersForJUnit, AssertionsForJUnit}

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */
class JsonStorageTestCase extends AssertionsForJUnit with ShouldMatchersForJUnit {

	object FooStorage extends JsonStorage[Foo] with BasicIsolation[Foo] {
		val location = "/tmp/test/foo"
	}

	@Before def cleanUp() {
    new Directory(FooStorage.location).clear()
  }

	@Test def update() {
		FooStorage.store(Foo(1))
		FooStorage.store(Foo(2))
    FooStorage.list.size should equal (2)

		val newData = Foo(1) :: Foo(3) :: Foo(4) :: Nil
		FooStorage.updateAll(newData)

    FooStorage.list.sorted should equal (newData)
	}

	@Test def isolation() {
		val isolated = FooStorage.isolated("x")
		isolated.store(Foo(1))

    isolated.list should equal (Foo(1)::Nil)
	}
}

case class Foo(@(Key@field) @(Attribute@field) var id: Int) extends Ordered[Foo] {
	def this() = this (0)

	def compare(that: Foo) = this.id.compare(that.id)
}