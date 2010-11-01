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

import org.junit.{Assert, Test}

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */
class RequestTestCase {
	@Test def pathParam = {
		val mapping = """/&year=(\d\d\d\d)/&month=(\w+)/&date=(\d+)"""
		Assert.assertEquals( "2009", Request.pathParam( mapping, "/2009/April/12", "year" ).get );
		Assert.assertEquals( "April", Request.pathParam( mapping, "/2009/April/12", "month" ).get );
		Assert.assertEquals( "12", Request.pathParam( mapping, "/2009/April/12", "date" ).get );
	}
}
