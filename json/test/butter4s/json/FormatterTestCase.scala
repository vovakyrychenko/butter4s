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

package butter4s.json

import org.junit.{Test, Assert}
import Assert._
import butter4s.benchmark._

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */
class FormatterTestCase {
	@Test def format = {
		val expected = "{\n\t\"a\": {\n\t\t\"xxxx\": \"y\\\" \\ry []\\n\\t\\t{}\"\n	},\n\t\"b\": [\n\t\t1,\n\t\t{\n\t\t\t\"xx\": null\n\t\t},\n\t\t3\n\t]\n}";
		val result = Formatter.format( "{\"a\": {\"xxxx\": \"y\\\" \\ry []\\n\\t\\t{}\"},\"b\":[1,{\"xx\":null},3]}" )
		assertEquals( expected, result )
		println( result )
	}

	@Test def performance = benchmark( "format", 1000 ) {
		Formatter.format( YearJson.json )
	}
}