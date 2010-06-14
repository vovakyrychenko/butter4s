package butter4s.servlet.rest

import javax.servlet.http.HttpServletResponse
import butter4s.servlet._
import butter4s.reflect._

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */
class RestServlet extends Servlet {
	override def post( request: Request, response: Response ) = get( request, response )

	override def get( request: Request, response: Response ) = {
		val action = request.getRequestURI.substring( request.getServletPath.length + 1 )
		val methodName = if ( action.contains( "/" ) ) action.substring( 0, action.indexOf( "/" ) ) else action
		getClass.declaredMethod( methodName ) match {
			case Some( method ) if ( method.annotatedWith[RestAction] ) => method.invoke( this, Array[AnyRef]( request, response ): _* )
			case Some( method ) => response.sendError( HttpServletResponse.SC_NOT_FOUND, methodName + " is not a @RestAction" )
			case None => response.sendError( HttpServletResponse.SC_NOT_FOUND, methodName + " not found" )
		}
	}
}