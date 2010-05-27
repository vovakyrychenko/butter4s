package butter4s.bind.json

import org.junit.Test

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */

class JSONTestCase {
	@Test def testEmptyList = {
		assert( JSON.parse( "[]" ) == Some( List() ) )
		assert( JSON.parse( "{}" ) == Some( Map[String, Any]() ) )
		assert( JSON.parse( "[{\"a\":[{}]}]" ) == Some( List( Map( "a" -> List( Map() ) ) ) ) )
	}
}