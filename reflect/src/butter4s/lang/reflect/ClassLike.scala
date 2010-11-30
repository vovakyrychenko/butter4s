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

import annotation.tailrec

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */
trait ClassLike[C] extends Type[C] {
	lazy val fields = {
		@tailrec def find( fields: List[Field], javaClass: Class[_] ): List[Field] =
			if ( javaClass == null ) fields else find( fields ::: javaClass.getDeclaredFields.map( new Field( _ ) ).toList, javaClass.getSuperclass )

		find( List[Field](), javaClass )
	}

	def field( name: String ) = fields.find( _.name == name )
}