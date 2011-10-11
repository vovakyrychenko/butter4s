package butter4s.servlet

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */

class Session( val impl: javax.servlet.http.HttpSession ) {
	def apply[A]( name: String ) = Option[A]( impl.getAttribute( name ).asInstanceOf[A] )

	def update( name: String, value: Any ) {
    impl.setAttribute(name, value)
  }
}