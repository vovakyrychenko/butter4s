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

import annotation.{Element, Attribute}
import butter4s.lang.reflect._
import butter4s.json.Parser.ParseException
import parameterized.TypeManifest

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */
class BasicBinder {
	private var marshallers = Map[parameterized.Type[_], AnyRef => String](
		typeOf[Enum[_]] -> ( value => "\"" + value.asInstanceOf[Enum[_]].name + "\"" )
		)

	trait Marshaller {
		def marshal( value: AnyRef ): String
	}

	def registerMarshaller( tm: parameterized.TypeManifest[_], m: Marshaller ) = marshallers += tm.asParameterizedType -> ( value => m.marshal( value ) )

	def registerMarshaller[A: Manifest]( m: AnyRef => String ) = marshallers += typeOf[A] -> m

	trait Unmarshaller {
		def unmarshal( t: parameterized.Type[_], value: Any ): Any
	}

	private var unmarshallers = Map[parameterized.Type[_], (parameterized.Type[_], Any) => Any](
		typeOf[Char] -> ( (_, value) => if ( value.asInstanceOf[String].length > 0 ) value.asInstanceOf[String]( 0 ) else 0: Char ),
		typeOf[Symbol] -> ( (_, value) => Symbol( value.asInstanceOf[String] ) ),
		typeOf[Enum[_]] -> ( (t, value) => t.as[parameterized.EnumType].rawType.valueOf( value.asInstanceOf[String] ) )
		)

	def registerUnmarshaller( tm: parameterized.TypeManifest[_], u: Unmarshaller ) = unmarshallers += tm.asParameterizedType -> ( (t, value) => u.unmarshal( t, value ) )

	def registerUnmarshaller[A: Manifest]( u: (parameterized.Type[_], Any) => Any ) = unmarshallers += typeOf[A] -> u

	def marshal( value: Any ): String = marshalUnknown( value )

	def marshalUnknown( value: Any ): String = value match {
		case null => "null"
		case v: String => "\"" + quote( v ) + "\""
		case v: Symbol => "\"" + quote( v.name ) + "\""
		case v: Char => "\"" + quote( v.toString ) + "\""
		case v: Int => v.toString
		case v: Long => v.toString
		case v: Float => v.toString
		case v: Double => v.toString
		case v: Byte => v.toString
		case v: Short => v.toString
		case v: Boolean => v.toString
		case v: List[_] => marshalList( v )
		case v: AnyRef => marshallers.find {case (t, _) => t.rawType <:< v.typeOf} match {
			case Some( (_, marshal) ) => marshal( v )
			case None => marshalObject( v )
		}
	}

	def marshalList( l: List[_] ) = "[" + l.map( e => marshalUnknown( e ) ).mkString( "," ) + "]"

	def marshalObject( obj: AnyRef ): String = "{" +
			obj.typeOf.as[raw.RefType].fields.filter( field => field.annotatedWith[Attribute] || field.annotatedWith[Element] )
					.map( field => marshalField( field, obj ) ).mkString( "," ) +
			"}"

	def marshalField[A]( field: raw.Field[A], obj: A ) = {
		val value = field.accessible {field.get( obj )}
		"\"" + field.name + "\":" +
				field.annotation[Element].flatMap( ann => if ( value != null && value.typeOf != field.rawType && ann.useTypeHint ) Some( "{\"value\":" + marshalUnknown( value ) + ",\"typeHint\":\"" + typeHint( value ) + "\"}" ) else None ).getOrElse( marshalUnknown( value ) )
	}

	private def typeHint( v: Any ) =
		if ( v.isInstanceOf[Int] ) "int"
		else if ( v.isInstanceOf[Long] ) "long"
		else if ( v.isInstanceOf[Short] ) "short"
		else if ( v.isInstanceOf[Byte] ) "byte"
		else if ( v.isInstanceOf[Float] ) "float"
		else if ( v.isInstanceOf[Double] ) "double"
		else if ( v.isInstanceOf[Boolean] ) "boolean"
		else if ( v.isInstanceOf[Char] ) "char"
		else if ( v.isInstanceOf[Symbol] ) "symbol"
		else if ( v.isInstanceOf[String] ) "string"
		else v.asInstanceOf[AnyRef].typeOf.name


	def unmarshal[A: Manifest]( input: String ): Option[A] = unmarshal( input, typeOf[A] ).asInstanceOf[Option[A]]

	def unmarshal[A]( input: String, tm: TypeManifest[A] ): Option[A] = unmarshal( input, tm.asParameterizedType ).asInstanceOf[Option[A]]

	def unmarshal( input: String, targetType: parameterized.Type[_] ): Option[Any] = try Some( unmarshalUnknown( targetType, Parser.parse( input ) ) ) catch {
		case e: ParseException => None
	}

	def unmarshalUnknown( targetType: parameterized.Type[_], value: Any ): Any =
		if ( value == null ) null
		else unmarshallers.find {case (t, _) => t.rawType <:< targetType.rawType} match {
			case Some( (_, unmarshal) ) => unmarshal( targetType, value )
			case None => value match {
				case v: String => v
				case v: Boolean => v
				case v: Double => unmarshalNumber( targetType, v )
				case v: List[Any] => unmarshalList( targetType, v )
				case v: Map[String, Any] => unmarshalObject( targetType.asInstanceOf[parameterized.ClassType[AnyRef]], v )
			}
		}

	def unmarshalList( targetType: parameterized.Type[_], list: List[Any] ) = list.map( unmarshalUnknown( targetType.arguments( 0 ), _ ) )

	def unmarshalNumber( targetType: parameterized.Type[_], v: Double ): AnyVal =
		if ( targetType.rawType <:< typeOf[Int].rawType ) v.toInt
		else if ( targetType.rawType <:< typeOf[java.lang.Integer].rawType ) v.toInt
		else if ( targetType.rawType <:< typeOf[Long].rawType ) v.toLong
		else if ( targetType.rawType <:< typeOf[java.lang.Long].rawType ) v.toLong
		else if ( targetType.rawType <:< typeOf[Short].rawType ) v.toShort
		else if ( targetType.rawType <:< typeOf[java.lang.Short].rawType ) v.toShort
		else if ( targetType.rawType <:< typeOf[Byte].rawType ) v.toByte
		else if ( targetType.rawType <:< typeOf[java.lang.Byte].rawType ) v.toByte
		else if ( targetType.rawType <:< typeOf[Float].rawType ) v.toFloat
		else if ( targetType.rawType <:< typeOf[java.lang.Float].rawType ) v.toFloat else v

	def unmarshalObject( targetType: parameterized.ClassType[AnyRef], map: Map[String, Any] ): AnyRef = {
		val obj = targetType.newInstance
		for ( (name, value) <- map ) targetType.fields.find( _.raw.name == name ) match {
			case None => throw new BindingException( "field " + name + " is not declared in " + targetType )
			case Some( field ) => field.raw.accessible {
				field.raw.set( obj,
					if ( field.raw.annotation[Element].flatMap( ann => if ( ann.useTypeHint ) Some( true ) else None ).isDefined )
						unmarshalUnknown( fromTypeHint( value.asInstanceOf[Map[String, Any]]( "typeHint" ).asInstanceOf[String] ), value.asInstanceOf[Map[String, Any]]( "value" ) )
					else unmarshalUnknown( field.actualType, value ) )
			}
		}
		obj
	}

	private def fromTypeHint( hint: String ) = hint match {
		case "int" => typeOf[Int]
		case "long" => typeOf[Long]
		case "short" => typeOf[Short]
		case "byte" => typeOf[Short]
		case "double" => typeOf[Double]
		case "float" => typeOf[Float]
		case "boolean" => typeOf[Boolean]
		case "char" => typeOf[Char]
		case "string" => typeOf[String]
		case "symbol" => typeOf[Symbol]
		case x => parameterized.Type.fromClass( Class.forName( x ) )
	}

	def unmarshalField[T <: parameterized.RefType[A], A]( field: parameterized.Field[T, A], obj: A, value: Any ) =
		field.raw.accessible {
			field.raw.set( obj,
				if ( field.raw.annotation[Element].flatMap( ann => if ( ann.useTypeHint ) Some( true ) else None ).isDefined )
					unmarshalUnknown( fromTypeHint( value.asInstanceOf[Map[String, Any]]( "typeHint" ).asInstanceOf[String] ), value.asInstanceOf[Map[String, Any]]( "value" ) )
				else unmarshalUnknown( field.actualType, value ) )
		}

	val dangerous = Map( '\\' -> "\\\\", '"' -> "\\\"", '\n' -> "\\n", '\r' -> "\\r", '\t' -> "\\t" )

	def quote( value: String ): String = value.foldLeft( "" )( (seed, c) => dangerous.get( c ) match {
		case Some( r ) => seed + r
		case None => seed + c
	} )
}

class BindingException( msg: String ) extends RuntimeException( msg )