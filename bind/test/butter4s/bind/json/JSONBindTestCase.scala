/**************************************************************************
 *
 * Copyright (c) Adstream Pty Ltd
 *
 * This software is the confidential and proprietary information of
 * Adstream Pty Ltd ("Confidential Information").  You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Adstream Pty Ltd.
 *
 ************************************************************************
 */

package butter4s.bind.json

import org.junit.{Assert, Test}
import annotation.target.field
import javax.xml.bind.annotation.{XmlAttribute, XmlElement}

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */
class JSONBindTestCase {
	@Test def bind = {
		val source = Bean("x", 10, Bean2("y", 15, List(1, 2, 3)))
		val json = JSONBind.marshal(source)
		println(json)
		val result = JSONBind.unmarshal[Bean](json)
		println(result)
		Assert.assertEquals(source, result)
	}

}

case class Bean(@(XmlElement@field) var str: String, @(XmlAttribute@field) var i: Int, @(XmlElement@field) var sb2: Bean2) {
	def this() = this (null, 0, null)
}

case class Bean2(@(XmlElement@field) var s2: String, @(XmlAttribute@field) var i2: Int, @(XmlElement@field) list: List[Int] = List()) {
	def this() = this (null, 0)
}

