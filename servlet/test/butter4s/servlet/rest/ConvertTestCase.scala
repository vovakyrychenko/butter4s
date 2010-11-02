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

package butter4s.servlet.rest

import javax.xml.bind.annotation.XmlAttribute
import annotation.target.field
import org.junit.{Assert, Test}
import butter4s.bind.json.JSONBind
import butter4s.reflect._

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */
class ConvertTestCase {
	@Test def convertJSON = Assert.assertEquals( Bean( "test" ), Convert.to( JSONBind.marshal( Bean( "test" ) ), MimeType.APPLICATION_JSON, classOf[Bean] ) )
}

case class Bean( @( XmlAttribute@field ) var name: String ) {
	def this() = this ( null )
}