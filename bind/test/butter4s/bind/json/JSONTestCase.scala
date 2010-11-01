package butter4s.bind.json

import org.junit.Test

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */

class JSONTestCase {
	@Test def testParse = {
		assert( JSON.parse( "[]" ) == Some( List() ) )
		assert( JSON.parse( "{}" ) == Some( Map[String, Any]() ) )
		assert( JSON.parse( "[{\"a\":[{}]}]" ) == Some( List( Map( "a" -> List( Map() ) ) ) ) )
		assert( JSON.parse( "true" ) == Some( true ) )
		assert( JSON.parse( "null" ) == Some( null ) )
		assert( JSON.parse( "123" ) == Some( 123.0 ) )
	}
}