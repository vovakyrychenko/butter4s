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

package butter4s.security

import sun.security.provider._
import sun.security.krb5.internal.crypto.crc32
import java.security.{MessageDigest, MessageDigestSpi, NoSuchAlgorithmException}
import java.lang.String
import java.io.{ByteArrayInputStream, InputStream}
import butter4s.lang._

object Digest {
	private var digests = Map[Symbol, () => MessageDigestSpi]()

	register( 'MD5, () => new MD5 )
	register( 'MD2, () => new MD2 )
	register( 'MD4, () => MD4.getInstance )
	register( 'SHA, () => new SHA )
	register( 'SHA2, () => new SHA2 )
	register( 'CRC32, () => new crc32 )


	def register( algorithm: Symbol, producer: () => MessageDigestSpi ) = digests.synchronized {digests += algorithm -> producer}

	def apply( algorithm: Symbol ) = getDigest( algorithm )

	def getDigest( algorithm: Symbol ) = digests.synchronized {
		val constructor = Class.forName( "java.security.MessageDigest$Delegate" ).getConstructor( classOf[MessageDigestSpi], classOf[String] )
		constructor.setAccessible( true )
		constructor.newInstance( digests.getOrElse( algorithm, throw new NoSuchAlgorithmException( algorithm + " is not available" ) )(), algorithm.toString ).asInstanceOf[MessageDigest]
	}

	def digest( algorithm: Symbol, is: InputStream ): String = {
		val digest = getDigest( algorithm )
		val buffer = new Array[Byte]( 1024 )
		var c: Int = 0
		while ( {c = is.read( buffer ); c != -1} ) digest.update( buffer, 0, c )
		digest.digest.toHexString
	}

	def digest( algorithm: Symbol, data: String ): String = digest( algorithm, new ByteArrayInputStream( data.getBytes( "UTF-8" ) ) )
}

