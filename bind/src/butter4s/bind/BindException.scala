package butter4s.bind

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */

class BindException( message: String, val data: String ) extends RuntimeException( message + ":\n" + data ) {
	def this( message: String ) = this ( message, null )
}