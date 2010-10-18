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

package butter4s.reflect

import org.junit.{Assert, Test}

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */
class ClazzTestCase {
	@Test def methodParam = {
		val parameters = classOf[ClazzTestCase].declaredMethod( "testM" ).get.parameters
		Assert.assertEquals( 1, parameters.size )
		val ann = parameters( 0 ).getAnnotation( classOf[TestAnn] )
		Assert.assertNotNull( ann )
		Assert.assertTrue( classOf[TestAnn] isInstance ann )
	}

	def testM( @TestAnn p1: String ) = null
}