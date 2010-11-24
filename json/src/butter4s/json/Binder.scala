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

import butter4s.reflect._
import javax.xml.bind.annotation.{XmlElement, XmlAttribute}
import javax.xml.bind.UnmarshalException
import java.lang.reflect.{TypeVariable, ParameterizedType, Type}
import butter4s.json.Parser.ParseException

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */

object Binder {
	private var marshallers = Map[Class[_], AnyRef => String](
		classOf[Enum[_]] -> ( value => "\"" + value.asInstanceOf[Enum[_]].name + "\"" )
		)

	trait Marshaller {
		def marshal( value: AnyRef ): String
	}

	def registerMarshaller( cls: Class[_], m: Marshaller ) = marshallers += cls -> ( value => m.marshal( value ) )

	def registerMarshaller[A: Manifest]( m: AnyRef => String ) = marshallers += manifest[A].erasure -> m

	trait Unmarshaller {
		def unmarshal( t: Type, value: Any ): Any
	}

	private var unmarshallers = Map[Type, (Type, Any) => Any]()

	class EnumUnmarshaller[E <: Enum[E]]( e: Class[E] ) extends ( (Type, Any) => Any ) with Unmarshaller {
		def unmarshal( t: Type, value: Any ) = apply( t, value )

		def apply( t: Type, value: Any ) = Enum.valueOf( e, value.asInstanceOf[String] )
	}

	def registerUnmarshaller( cls: Class[_], u: Unmarshaller ) = unmarshallers += cls -> ( (t, value) => u.unmarshal( t, value ) )

	def registerUnmarshaller[A: Manifest]( u: (Type, Any) => Any ) = unmarshallers += manifest[A].erasure -> u

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
		case v: AnyRef => marshallers.find {case (c, _) => c.isAssignableFrom( v.getClass )} match {
			case Some( (_, marshal) ) => marshal( v )
			case None => marshalObject( v )
		}
	}

	def marshalList( l: List[_] ) = "[" + l.map( e => marshalUnknown( e ) ).mkString( "," ) + "]"

	def marshalObject( obj: AnyRef ): String = "{" +
			obj.getClass.declaredFields.filter( field => field.annotatedWith[XmlElement] || field.annotatedWith[XmlAttribute] )
					.map( field => "\"" + field.name + "\":" + marshalUnknown( field.get( obj ) ) ).mkString( "," ) +
			"}"

	def unmarshal[A: Manifest]( input: String ): Option[A] = unmarshal( input, manifest[A].asParameterizedType ).asInstanceOf[Option[A]]

	def unmarshal( input: String, targetType: Type ): Option[Any] = try Some( unmarshalUnknown( targetType, Parser.parse( input ) ) ) catch {
		case e: ParseException => None
	}

	def unmarshalUnknown( targetType: Type, value: Any ): Any =
		if ( value == null ) null
		else unmarshallers.find {case (t, _) => targetType.assignableFrom( t.toClazz[AnyRef] )} match {
			case Some( (_, unmarshal) ) => unmarshal( targetType, value )
			case None => value match {
				case v: String => v
				case v: Boolean => v
				case v: Double => unmarshalNumber( targetType, v )
				case v: List[Any] => unmarshalList( targetType.asInstanceOf[ParameterizedType], v )
				case v: Map[String, Any] => unmarshalObject( targetType, v )
			}
		}

	def unmarshalList( targetType: ParameterizedType, list: List[Any] ) = list.map( unmarshalUnknown( targetType.getActualTypeArguments()( 0 ), _ ) )

	def unmarshalNumber( targetType: Type, v: Double ): AnyVal =
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

	def unmarshalObject( targetType: Type, map: Map[String, Any] ): AnyRef = {
		val clazz = targetType.toClazz[AnyRef]
		val obj = clazz.newInstance
		for ( (name, value) <- map ) clazz.declaredField( name ) match {
			case None => throw new UnmarshalException( "field " + name + " is not declared in " + clazz )
			case Some( field ) => field.set( obj, unmarshalUnknown( if ( field.getGenericType.isInstanceOf[TypeVariable[_]] ) field.getGenericType.resolveWith( targetType.asInstanceOf[ParameterizedType] ) else field.getGenericType, value ) )
		}
		obj
	}

	val dangerous = Map( '\\' -> "\\\\", '"' -> "\\\"", '\n' -> "\\n", '\r' -> "\\r", '\t' -> "\\t" )

	def quote( value: String ): String = value.foldLeft( "" )( (seed, c) => dangerous.get( c ) match {
		case Some( r ) => seed + r
		case None => seed + c
	} )
}

