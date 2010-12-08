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
package butter4s.net.http.rest

import java.io.Writer

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */
object Response {
	object Code {
		final val CONTINUE = 100;
		final val SWITCHING_PROTOCOLS = 101;
		final val OK = 200;
		final val CREATED = 201;
		final val ACCEPTED = 202;
		final val NON_AUTHORITATIVE_INFORMATION = 203;
		final val NO_CONTENT = 204;
		final val RESET_CONTENT = 205;
		final val PARTIAL_CONTENT = 206;
		final val MULTIPLE_CHOICES = 300;
		final val MOVED_PERMANENTLY = 301;
		final val MOVED_TEMPORARILY = 302;
		final val FOUND = 302;
		final val SEE_OTHER = 303;
		final val NOT_MODIFIED = 304;
		final val USE_PROXY = 305;
		final val TEMPORARY_REDIRECT = 307;
		final val BAD_REQUEST = 400;
		final val UNAUTHORIZED = 401;
		final val PAYMENT_REQUIRED = 402;
		final val FORBIDDEN = 403;
		final val NOT_FOUND = 404;
		final val METHOD_NOT_ALLOWED = 405;
		final val NOT_ACCEPTABLE = 406;
		final val PROXY_AUTHENTICATION_REQUIRED = 407;
		final val REQUEST_TIMEOUT = 408;
		final val CONFLICT = 409;
		final val GONE = 410;
		final val LENGTH_REQUIRED = 411;
		final val PRECONDITION_FAILED = 412;
		final val REQUEST_ENTITY_TOO_LARGE = 413;
		final val REQUEST_URI_TOO_LONG = 414;
		final val UNSUPPORTED_MEDIA_TYPE = 415;
		final val REQUESTED_RANGE_NOT_SATISFIABLE = 416;
		final val EXPECTATION_FAILED = 417;
		final val INTERNAL_SERVER_ERROR = 500;
		final val NOT_IMPLEMENTED = 501;
		final val BAD_GATEWAY = 502;
		final val SERVICE_UNAVAILABLE = 503;
		final val GATEWAY_TIMEOUT = 504;
		final val HTTP_VERSION_NOT_SUPPORTED = 505;
	}
}

trait Response {
	def content( contentType: String, what: ( => Writer ) => Unit ): Unit

	def status( code: Int, message: String = null ): Unit
}
