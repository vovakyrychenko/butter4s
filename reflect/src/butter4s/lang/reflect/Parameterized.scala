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
trait Parameterized[A] extends Type[A] {
	val arguments: List[Type[_]]
}

trait ManifestParameterized[A] extends Parameterized[A] {
	val manifest: Manifest[A]

	lazy val arguments = manifest.typeArguments.map( _.asType )
}

trait NativeParameterized[A] extends Parameterized[A] {
	val javaType: java.lang.reflect.ParameterizedType

	lazy val arguments = javaType.getActualTypeArguments.map( _.asType )
}