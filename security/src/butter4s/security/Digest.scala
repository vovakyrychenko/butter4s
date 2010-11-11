/*
 * The MIT License
 *
 * Copyright (c) 2010 Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
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

