package butter4s.servlet

import rest.RestRequest

package object rest {
	implicit def unconvertRequest( request: RestRequest ) = request.impl
}