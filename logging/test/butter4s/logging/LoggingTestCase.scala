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

package butter4s.logging

import org.junit.Test

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */

class LoggingTestCase extends Logging {
	@Test
	def test = {
		debug( "test" )
		info( "info" )
		log.error( "error" )
		fatal( "fatal", new RuntimeException( "error" ) )
	}
}