/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package butter4s.bind.json

/**
 * This object provides a simple interface to the JSON parser class.
 *
 * @author Derek Chen-Becker <"java"+@+"chen-becker"+"."+"org">
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */
object JSON extends Parser {

	/**
	 * Parse the given JSON string and return parsed value.
	 *
	 * @param input the given JSON string.
	 * @return an optional list of of elements.
	 */
	def parse( input: String ): Option[Any] = phrase( root )( new lexical.Scanner( input ) ) match {
		case Success( result, _ ) => Some( result )
		case _ => None
	}
}
