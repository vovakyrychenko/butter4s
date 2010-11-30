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
package butter4s.json

import butter4s.lang.reflect._
import javax.xml.bind.annotation.{XmlElement, XmlAttribute}
import javax.xml.bind.UnmarshalException
//import java.lang.reflect.{TypeVariable, ParameterizedType, Type}
import butter4s.json.Parser.ParseException

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */

object Binder2 {
	private var marshallers = Map[Type[_], AnyRef => String](
		typeOf[Enum[_]] -> ( value => "\"" + value.asInstanceOf[Enum[_]].name + "\"" )
		)

	trait Marshaller {
		def marshal( value: AnyRef ): String
	}

	def registerMarshaller( cls: Class[_], m: Marshaller ) = marshallers += cls.asType -> ( value => m.marshal( value ) )

	def registerMarshaller[A: Manifest]( m: AnyRef => String ) = marshallers += typeOf[A] -> m

	trait Unmarshaller {
		def unmarshal( t: Type[_], value: Any ): Any
	}

	private var unmarshallers = Map[Type[_], (Type[_], Any) => Any]()

	class EnumUnmarshaller[E <: Enum[E]]( e: Class[E] ) extends ( (Type[_], Any) => Any ) with Unmarshaller {
		def unmarshal( t: Type[_], value: Any ) = apply( t, value )

		def apply( t: Type[_], value: Any ) = Enum.valueOf( e, value.asInstanceOf[String] )
	}

	def registerUnmarshaller( cls: Class[_], u: Unmarshaller ) = unmarshallers += cls.asType -> ( (t, value) => u.unmarshal( t, value ) )

	def registerUnmarshaller[A: Manifest]( u: (Type[_], Any) => Any ) = unmarshallers += typeOf[A] -> u

	def marshal( value: Any ): String = marshalUnknown( value )

	def marshalUnknown( value: Any ): String = value match {
		case null => "null"
		case v: String => "\"" + quote( v ) + "\""
		case v: Int => v.toString
		case v: Long => v.toString
		case v: Float => v.toString
		case v: Double => v.toString
		case v: Byte => v.toString
		case v: Short => v.toString
		case v: Boolean => v.toString
		case v: List[_] => marshalList( v )
		case v: AnyRef => marshallers.find {case (t, _) => t <:< v.typeOf} match {
			case Some( (_, marshal) ) => marshal( v )
			case None => marshalObject( v )
		}
	}

	def marshalList( l: List[_] ) = "[" + l.map( e => marshalUnknown( e ) ).mkString( "," ) + "]"

	def marshalObject( obj: AnyRef ): String = "{" +
			obj.typeOf.asInstanceOf[ClassType[_]].fields.filter( field => field.annotatedWith[XmlElement] || field.annotatedWith[XmlAttribute] )
					.map( field => "\"" + field.name + "\":" + marshalUnknown( field.accessible( field.get( obj ) ) ) ).mkString( "," ) +
			"}"

	def unmarshal[A: Manifest]( input: String ): Option[A] = unmarshal( input, typeOf[A] ).asInstanceOf[Option[A]]

	def unmarshal( input: String, targetType: Type[_] ): Option[Any] = try Some( unmarshalUnknown( targetType, Parser.parse( input ) ) ) catch {
		case e: ParseException => None
	}

	def unmarshalUnknown( targetType: Type[_], value: Any ): Any =
		if ( value == null ) null
		else unmarshallers.find {case (t, _) => targetType <:< t} match {
			case Some( (_, unmarshal) ) => unmarshal( targetType, value )
			case None => value match {
				case v: String => v
				case v: Boolean => v
				case v: Double => unmarshalNumber( targetType, v )
				case v: List[_] => unmarshalList( targetType.asInstanceOf[Parameterized[_]], v )
				case v: Map[String, _] => unmarshalObject( targetType, v )
			}
		}

	def unmarshalList( targetType: Parameterized[_], list: List[_] ) = list.map( unmarshalUnknown( targetType.arguments( 0 ), _ ) )

	def unmarshalNumber( targetType: Type[_], v: Double ): AnyVal =
		if ( targetType.assignableFrom[Int] ) v.toInt
		else if ( targetType.assignableFrom[java.lang.Integer] ) v.toInt
		else if ( targetType.assignableFrom[Long] ) v.toLong
		else if ( targetType.assignableFrom[java.lang.Long] ) v.toLong
		else if ( targetType.assignableFrom[Short] ) v.toShort
		else if ( targetType.assignableFrom[java.lang.Short] ) v.toShort
		else if ( targetType.assignableFrom[Byte] ) v.toByte
		else if ( targetType.assignableFrom[java.lang.Byte] ) v.toByte
		else if ( targetType.assignableFrom[Float] ) v.toFloat
		else if ( targetType.assignableFrom[java.lang.Float] ) v.toFloat else v

	def unmarshalObject( targetType: Type[_], map: Map[String, Any] ): AnyRef = {
		val classType = targetType.asInstanceOf[ClassType[_]]
		val obj = classType.newInstance
		for ( (name, value) <- map ) classType.field( name ) match {
			case None => throw new UnmarshalException( "field " + name + " is not declared in " + clazz )
			case Some( field ) => field.accessible( field.set( obj, unmarshalUnknown( field.actualType( targetType.asInstanceOf[Parameterized] ), value ) ) )
		}
		obj
	}

	val dangerous = Map( '\\' -> "\\\\", '"' -> "\\\"", '\n' -> "\\n", '\r' -> "\\r", '\t' -> "\\t" )

	def quote( value: String ): String = value.foldLeft( "" )( (seed, c) => dangerous.get( c ) match {
		case Some( r ) => seed + r
		case None => seed + c
	} )
}

