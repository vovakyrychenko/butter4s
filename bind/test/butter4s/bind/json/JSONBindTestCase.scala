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

import butter4s.bind.{ListElementType, Element, Attribute, Bindable}
import org.junit.{Assert, Test}

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */
class JSONBindTestCase {
	@Test def bind = {
		val source = Bean( "x", 10, Bean2( "y", 15, List( 1, 2, 3 ) ) )
		val json = JSONBind.marshal( source )
		println( json )
		val result = JSONBind.unmarshal[Bean]( json )
		println( result )
		Assert.assertEquals( source, result )
	}

}

@Bindable
case class Bean( @Element var str: String, @Attribute var i: Int, @Element var sb2: Bean2 ) {
	def this() = this ( null, 0, null )
}

@Bindable
case class Bean2( @Element var s2: String, @Attribute var i2: Int, @ListElementType( classOf[Int] ) @Element list: List[Int] = List() ) {
	def this() = this ( null, 0 )
}

