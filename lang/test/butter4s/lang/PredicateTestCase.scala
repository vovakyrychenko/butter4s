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

package butter4s.lang

import org.junit.{Test, Assert}

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */
class PredicateTestCase {
	@Test
	def testAndOr = {
		val p1 = (s: String) => true
		val p2 = (s: AnyRef) => true

		Assert.assertTrue( ( p1 && p2 )( "" ) )
		Assert.assertFalse( ( p1 && not( p2 ) )( "" ) )

		Assert.assertTrue( ( not( p1 ) || p2 )( "" ) )
		Assert.assertFalse( ( not( p1 ) || not( p2 ) )( "" ) )

		println( not( p1 ) || p2 )
	}
}