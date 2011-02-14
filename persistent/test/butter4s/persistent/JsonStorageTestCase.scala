package butter4s.persistent

import org.junit.Assert._
import butter4s.json.annotation.Attribute
import annotation.target.field
import butter4s.fs.Directory
import org.junit.{Before, Test}

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

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */
class JsonStorageTestCase {

	object FooStorage extends JsonStorage[ Foo ] {
		val location = "/tmp/test/foo"
	}

	@Before def cleanUp = new Directory(FooStorage.location).clear

	@Test def update = {
		FooStorage.store(Foo(1))
		FooStorage.store(Foo(2))
		assertEquals(2, FooStorage.list.size)
		val newData = Foo(1) :: Foo(3) :: Foo(4) :: Nil
		FooStorage.updateAll(newData)
		assertEquals(newData, FooStorage.list.sorted)
	}
}

case class Foo(@( Key@field ) @( Attribute@field ) var id: Int) extends Ordered[ Foo ] {
	def this() = this (0)

	def compare(that: Foo) = this.id.compare(that.id)
}