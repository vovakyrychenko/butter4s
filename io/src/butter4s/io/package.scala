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
package butter4s

import java.io.{ByteArrayOutputStream, InputStream, OutputStream}

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */
package object io {
	val EOS = -1;

	def using[A <: {def close()}, B]( c: A )( body: A => B ) = try body( c ) finally c.close


	class RichOutputStream( out: OutputStream ) {
		def write[P]( source: P )( implicit convert: io.ToArrayConversion[P] ) = out.write( convert( source ) )
	}

	implicit def toRichOutputStream( out: OutputStream ) = new RichOutputStream( out )

	class RichInputStream( in: InputStream ) {
		def readAs[R]( implicit convert: io.FromArrayConversion[R] ): R = {
			val bytes = new ByteArrayOutputStream()
			copy( bytes )
			convert( bytes.toByteArray )
		}

		def copy( out: OutputStream ) = {
			val buffer = new Array[Byte]( 2048 );
			var count = 0
			while ( {count = in.read( buffer ); count != EOS} ) out.write( buffer, 0, count );
			out.flush();
		}
	}

	implicit def toRichInputStream( in: InputStream ) = new RichInputStream( in )
}

package io {
object FromArrayConversion extends LowPriorityImplicits
trait FromArrayConversion[R] extends ( Array[Byte] => R )


object ToArrayConversion extends LowPriorityImplicits
trait ToArrayConversion[P] extends ( P => Array[Byte] )


trait LowPriorityImplicits {
	implicit object ToBytesArrayConversion extends FromArrayConversion[Array[Byte]] {
		def apply( bytes: Array[Byte] ) = bytes
	}

	implicit object ToStringArrayConversion extends FromArrayConversion[String] {
		def apply( bytes: Array[Byte] ) = new String( bytes, "UTF-8" )
	}

	implicit object ToByteArrayConversion extends ToArrayConversion[String] {
		def apply( s: String ) = s.getBytes( "UTF-8" )
	}
}
}