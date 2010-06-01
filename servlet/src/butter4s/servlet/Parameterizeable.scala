package butter4s.servlet

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */

trait Parameterizeable {
	val impl: {def getInitParameter( name: String ): String}

	def apply( name: String ) = Option( impl.getInitParameter( name ) )
}