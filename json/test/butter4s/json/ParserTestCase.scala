package butter4s.json

import butter4s.benchmark._
import org.junit.{Assert, Test}
import Assert._

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */

class ParserTestCase {
	@Test def parse: Unit = {
		assertEquals( true, Parser.parse( "true" ) )
		assertEquals( false, Parser.parse( "false" ) )
		assertNull( Parser.parse( "null" ) )
		assertEquals( 123.0, Parser.parse( "123" ) )
		assertEquals( 123.5, Parser.parse( "123.5" ) )
		assertEquals( "str", Parser.parse( "\"str\"" ) )
		assertEquals( List(), Parser.parse( "[]" ) )
		assertEquals( List( 1., 2., 3.), Parser.parse( "[1,2,3]" ) )
		assertEquals( List( 1., List( 1., 2.), 3.), Parser.parse( "[1,[1,2],3]" ) )
		assertEquals( Map[String, Any](), Parser.parse( "{}" ) )
		assertEquals( List( Map( "a" -> List( Map() ) ) ), Parser.parse( "[{\"a\":[{}]}]" ) )
		println( Parser.parse( YearJson.json ) )
	}

	@Test def performance: Unit = benchmark( "parser", 500 ) {
		Parser.parse( YearJson.json )
	}
}