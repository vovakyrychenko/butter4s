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
package butter4s.bind.json

import butter4s.reflect._
import javax.xml.bind.annotation.{XmlElement, XmlAttribute}
import javax.xml.bind.UnmarshalException
import java.lang.reflect.{TypeVariable, ParameterizedType, Type}
import butter4s.bind.json.JsonAST._
import butter4s.bind.json.JsonParser.ParseException

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */

object JsonBind {
	def marshal( value: Any ): String = marshalUnknown( "\t", value )

	private def marshalUnknown( tab: String, value: Any ): String = value match {
		case null => "null"
		case v: String => "\"" + quote( v ) + "\""
		case v: Int => v.toString
		case v: Long => v.toString
		case v: Float => v.toString
		case v: Double => v.toString
		case v: Byte => v.toString
		case v: Short => v.toString
		case v: Boolean => v.toString
		case v: List[_] => "[\n" + v.map( e => tab + marshalUnknown( tab + "\t", e ) ).mkString( ",\n" ) + "\n" + tab.substring( 1 ) + "]"
		case v: AnyRef => marshalObject( tab, v )
	}

	private def marshalObject( tab: String, obj: AnyRef ): String =
		"{\n" + obj.getClass.declaredFields.filter( field => field.annotatedWith[XmlElement] || field.annotatedWith[XmlAttribute] )
				.map( field => tab + "\"" + field.name + "\": " + marshalUnknown( tab + "\t", field.get( obj ) ) ).mkString( ",\n" ) +
				"\n" + tab.substring( 1 ) + "}"

	def unmarshal[A: Manifest]( input: String ): Option[A] = unmarshal( input, manifest[A].asParameterizedType ).asInstanceOf[Option[A]]

	def unmarshal( input: String, t: Type ): Option[Any] =
		if ( input.startsWith( "\"" ) && input.endsWith( "\"" ) ) Some( input )
		else if ( "null".equals( input.trim ) ) Some( null )
		else try Some( unmarshalNumber( t, input.toDouble ) ) catch {
			case e: NumberFormatException =>
				try Some( input.toBoolean ) catch {
					case e: NumberFormatException =>
						try Some( unmarshalUnknown( t, JsonParser.parse( input ) ) ) catch {case e: ParseException => None}
				}
		}

	private def unmarshalUnknown( t: Type, value: JValue ): Any = value match {
		case JNull => null
		case JString( v ) => v
		case JBool( v ) => v
		case JDouble( v ) => unmarshalNumber( t, v )
		case JInt( v ) => unmarshalNumber( t, v.toDouble )
		case JArray( v ) => unmarshalList( t.asInstanceOf[ParameterizedType], v )
		case JObject( v ) => unmarshalObject( t, v )
		case _ => throw new UnmarshalException( "should not happen" )
	}

	private def unmarshalList( t: ParameterizedType, list: List[JValue] ) = list.map( unmarshalUnknown( t.getActualTypeArguments()( 0 ), _ ) )

	private def unmarshalNumber( t: Type, v: Double ): AnyVal =
		if ( t.assignableFrom[Int] ) v.toInt
		else if ( t.assignableFrom[java.lang.Integer] ) v.toInt
		else if ( t.assignableFrom[Long] ) v.toLong
		else if ( t.assignableFrom[java.lang.Long] ) v.toLong
		else if ( t.assignableFrom[Short] ) v.toShort
		else if ( t.assignableFrom[java.lang.Short] ) v.toShort
		else if ( t.assignableFrom[Byte] ) v.toByte
		else if ( t.assignableFrom[java.lang.Byte] ) v.toByte
		else if ( t.assignableFrom[Float] ) v.toFloat
		else if ( t.assignableFrom[java.lang.Float] ) v.toFloat else v

	private def unmarshalObject( t: Type, fields: List[JField] ): AnyRef = {
		val clazz = t.toClass[AnyRef]
		val obj = clazz.newInstance
		for ( JField( name, value ) <- fields ) clazz.declaredField( name ) match {
			case None => throw new UnmarshalException( "field " + name + " is not declared in " + clazz )
			case Some( field ) => field.set( obj, unmarshalUnknown( if ( field.getGenericType.isInstanceOf[TypeVariable[_]] ) field.getGenericType.resolveWith( t.asInstanceOf[ParameterizedType] ) else field.getGenericType, value ) )
		}
		obj
	}

	val dangerous = Map( '\\' -> "\\\\", '"' -> "\\\"", '\n' -> "\\n", '\r' -> "\\r", '\t' -> "\\t" )

	def quote( value: String ): String = value.foldLeft( "" )( (seed, c) => dangerous.get( c ) match {
		case Some( r ) => seed + r
		case None => seed + c
	} )
}

