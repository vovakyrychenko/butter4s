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

package butter4s.net.http.rest.embedded

import butter4s.net.http.rest


/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */
object ServerTest extends Application {
	val server = new Server( 9000 )
	server.add( MathService )
	server.start
}

object MathService extends rest.Service {
	def serviceName = "math"

	@rest.Method( produces = rest.MimeType.APPLICATION_JSON )
	def sum( @rest.Param( name = "a" ) a: Int, @rest.Param( name = "b" ) b: Int ) = a + b
}