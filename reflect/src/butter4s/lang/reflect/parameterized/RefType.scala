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
package butter4s.lang.reflect.parameterized

import annotation.tailrec
import butter4s.lang.reflect.{raw => rawtypes}

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */

trait RefType[ T ] extends Type[ T ] {
	lazy val fields = {
		@tailrec def find(fields: List[ Field[ RefType.this.type, T ] ], clazz: Class[ _ ]): List[ Field[ RefType.this.type, T ] ] =
			if( clazz == null ) fields else find(fields ::: clazz.getDeclaredFields.map(new Field[ RefType.this.type, T ](_, RefType.this)).toList, clazz.getSuperclass)

		find(List[ Field[ RefType.this.type, T ] ](), javaClass)
	}

	lazy val methods = {
		@tailrec def find(methods: List[ Method[ RefType.this.type, T ] ], clazz: Class[ _ ]): List[ Method[ RefType.this.type, T ] ] =
			if( clazz == null ) methods else find(methods ::: clazz.getDeclaredMethods.map(new Method[ RefType.this.type, T ](_, RefType.this)).toList, clazz.getSuperclass)

		find(List[ Method[ RefType.this.type, T ] ](), javaClass)
	}
}


class Field[ OT <: RefType[ T ], T ] private[ parameterized ](protected val javaField: java.lang.reflect.Field, val ownerType: OT) {
	lazy val raw = new rawtypes.Field[ T ](javaField)

	lazy val actualType: Type[ _ ] = Type.actualize(javaField.getGenericType, ownerType)

	override def toString = "field " + raw.name
}

class Method[ OT <: RefType[ T ], T ] private[ parameterized ](protected val javaMethod: java.lang.reflect.Method, val ownerType: OT) {
	lazy val raw = new rawtypes.Method[ T ](javaMethod)

	lazy val parameters = javaMethod.getParameterTypes.zip(javaMethod.getGenericParameterTypes).zip(javaMethod.getParameterAnnotations).map {
		case ((cls, t), as) => new Parameter[ OT, T ](cls, t, as.toList, Method.this)
	}

	override def toString = "method " + raw.name
}

class Parameter[ OT <: RefType[ T ], T ] private[ parameterized ](protected val javaClass: Class[ _ ], protected val nativeType: java.lang.reflect.Type, val annotations: List[ java.lang.annotation.Annotation ], val ownerMethod: Method[ OT, T ]) {
	lazy val raw = new rawtypes.Parameter(javaClass, annotations)

	lazy val actualType: Type[ _ ] = Type.actualize(nativeType, ownerMethod.ownerType)

	override def toString = "parameter " + nativeType
}
