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

package butter4s.logging

import org.apache.commons.logging.{Log, LogFactory}
import compat.Platform

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */

trait Logging {
	private val impl: Log = LogFactory.getLog( this.getClass )

	def trace( any: => Any ) = if ( impl.isTraceEnabled ) impl.trace( any )

	def trace( any: => Any, error: => Throwable ) = if ( impl.isTraceEnabled ) impl.trace( any, error )

	def debug( any: => Any ) = if ( impl.isDebugEnabled ) impl.debug( any )

	def debug( any: => Any, error: => Throwable ) = if ( impl.isDebugEnabled ) impl.debug( any, error )

	def info( any: => Any ) = if ( impl.isInfoEnabled ) impl.info( any )

	def info( any: => Any, error: => Throwable ) = if ( impl.isInfoEnabled ) impl.info( any, error )

	def warn( any: => Any ) = if ( impl.isWarnEnabled ) impl.warn( any )

	def warn( any: => Any, error: => Throwable ) = if ( impl.isWarnEnabled ) impl.warn( any, error )

	def error( any: => Any ) = if ( impl.isErrorEnabled ) impl.error( any )

	def error( any: => Any, error: => Throwable ) = if ( impl.isErrorEnabled ) impl.error( any, error )

	def fatal( any: => Any ) = if ( impl.isFatalEnabled ) impl.fatal( any )

	def fatal( any: => Any, error: => Throwable ) = if ( impl.isFatalEnabled ) impl.fatal( any, error )

	def time[A]( message: String, f: => A ): A = {
		val start = Platform.currentTime
		val result = f
		debug( message + " took " + ( Platform.currentTime - start ) + " ms" )
		result
	}

	val log = this;
} 