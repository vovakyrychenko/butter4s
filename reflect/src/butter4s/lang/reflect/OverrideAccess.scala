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

package butter4s.lang.reflect

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */
trait OverrideAccess[T] {
	def setAccessible( a: Boolean ): Unit

	def accessible[A]( code: => A ) = {
		setAccessible( true )
		code
	}
}