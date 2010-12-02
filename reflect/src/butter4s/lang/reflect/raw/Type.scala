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
package butter4s.lang.reflect.raw

import butter4s.lang.reflect.TypeVariable

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */
object Type {
	def fromClass[A]( javaClass: Class[A] ) =
		if ( javaClass.isAnnotation ) new AnnotationType[A]( javaClass )
		else if ( javaClass.isEnum ) new EnumType[A]( javaClass )
		else if ( javaClass.isArray ) new ArrayType[A]( javaClass )
		else if ( javaClass.isInterface ) new InterfaceType[A]( javaClass )
		else if ( javaClass.isPrimitive ) new PrimitiveType[A]( javaClass )
		else new ClassType[A]( javaClass )
}

trait Type[T] {
	protected val javaClass: Class[T]
	lazy val name = javaClass.getName
	lazy val simpleName = javaClass.getSimpleName
	lazy val parameters = javaClass.getTypeParameters.map( new TypeVariable( _ ) ).toList

	def as[RT[x] <: Type[x]] = this.asInstanceOf[RT[T]]

	def <:<( that: Type[_] ) = javaClass.isAssignableFrom( that.javaClass )

	override def hashCode = javaClass.##

	override def equals( that: Any ) = that.isInstanceOf[Type[_]] && javaClass == that.asInstanceOf[Type[_]].javaClass

	override def toString = getClass.getSimpleName + "(" + simpleName + ")"
}

class ClassType[T] private[reflect]( protected val javaClass: Class[T] ) extends RefType[T]
class InterfaceType[T] private[reflect]( protected val javaClass: Class[T] ) extends RefType[T]
class AnnotationType[T] private[reflect]( protected val javaClass: Class[T] ) extends RefType[T]
class EnumType[T] private[reflect]( protected val javaClass: Class[T] ) extends RefType[T] {
	lazy val values = javaClass.getEnumConstants.toList

	def valueOf( v: String ) = values.find( _.asInstanceOf[Enum[_]].name == v ) match {
		case Some( e ) => e
		case None => throw new IllegalArgumentException( "No enum const " + name + "." + v )
	}
}
class ArrayType[T] private[reflect]( protected val javaClass: Class[T] ) extends RefType[T]
class PrimitiveType[T] private[reflect]( protected val javaClass: Class[T] ) extends Type[T]
