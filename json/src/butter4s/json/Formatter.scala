/**************************************************************************
 *
 * Copyright (c) Adstream Holdings Pty Ltd
 *
 * This software is the confidential and proprietary information of
 * Adstream Holdings Pty Ltd ("Confidential Information").  You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Adstream Holdings Pty Ltd.
 *
 ************************************************************************
 */

package butter4s.json

import scala.collection.mutable.StringBuilder
import scala.annotation.switch

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */
object Formatter {
	def format( json: String ) = {
		val b = new StringBuilder
		var string = false
		var escape = false
		var tabs = new StringBuilder
		for ( c <- json ) ( c: @switch ) match {
			case '"' if !escape => b.append( "\"" ); string = !string; escape = false
			case x@( '{' | '[' ) if !string => tabs += '\t'; b.append( x ).append( "\n" ).append( tabs ); escape = false
			case x@( '}' | ']' ) if !string => tabs.deleteCharAt( tabs.length - 1 ); b.append( "\n" ).append( tabs ).append( x ); escape = false
			case x@',' if !string => b.append( x ).append( "\n" ).append( tabs ); escape = false
			case x@'\\' if string => b.append( x ); escape = !escape
			case x@':' if !string => b.append( x ).append( " " ); escape = false
			case '\n' | '\r' | '\t' | ' ' if !string =>
			case x => b.append( x ); escape = false
		}
		b.toString
	}
}