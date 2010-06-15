/**************************************************************************
 *
 * Copyright (c) Adstream Pty Ltd
 *
 * This software is the confidential and proprietary information of
 * Adstream Pty Ltd ("Confidential Information").  You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Adstream Pty Ltd.
 *
 ************************************************************************
 */

package butter4s.servlet.rest

import butter4s.servlet._

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */
class RestRequest( private[rest] val impl: Request, path: String ) {
	lazy val query = impl.getRequestURI.substring( impl.getServletPath.length + 1 + path.length + ( if ( path.endsWith( "/" ) ) 0 else 1 ) )
}