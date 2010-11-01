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
import javax.xml.bind.{MarshalException, UnmarshalException}
import java.lang.reflect.{ParameterizedType, Type}

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */
object JSONBind {
	def marshal( a: AnyRef ): String = if ( a == null ) "null" else marshalObject( "\t", a.getClass, a )

	def marshalObject( tab: String, actualType: Type, a: AnyRef ): String = {
		val fields: List[Field] = a.getClass.declaredFields.filter( field => field.annotatedWith[XmlElement] || field.annotatedWith[XmlAttribute] )
		"{\n" +
				fields.map( field => tab + "\"" + field.name + "\": " + marshalValue( tab + "\t", field.getGenericType.resolveWith( actualType ), field.get( a ) ) ).mkString( ",\n" ) +
				"\n" + tab.substring( 1 ) + "}"
	}

	def marshalValue( tab: String, t: Type, value: Any ): String = {
		def _marshal( t: Type, filter: Class[_] => Boolean ) = marshallers.find( t => filter( t._1 ) ) match {
			case Some( (_, m) ) => m.marshal( tab, value, t )
			case None if value.isInstanceOf[AnyRef] => marshalObject( tab, t, value.asInstanceOf[AnyRef] )
			case _ => throw new MarshalException( "could not marshal " + value + " as type " + t )
		}

		if ( value == null ) "null" else t match {
			case clazz: Class[_] => _marshal( clazz, c => clazz.isAssignableFrom( c ) )
			case pt: ParameterizedType => _marshal( pt, c => pt.getRawType.asInstanceOf[Class[_]].isAssignableFrom( c ) )
			case t => throw new MarshalException( "unresolved type " + t + ":" + t.getClass + " for object " + value )
		}
	}

	def unmarshal[A <: AnyRef : Manifest]( input: String ): A = JSON.parse( input ) match {
		case None => throw new UnmarshalException( "could not parse", input )
		case Some( map ) => unmarshal[A]( manifest[A].erasure.asInstanceOf[Class[A]], map.asInstanceOf[Map[String, Any]] )
	}

	def unmarshal[A <: AnyRef]( input: String, t: Type ): A = JSON.parse( input ) match {
		case None => throw new UnmarshalException( "could not parse", input )
		case Some( map ) => unmarshal[A]( t, map.asInstanceOf[Map[String, Any]] )
	}

	def unmarshal[A <: AnyRef]( t: Type, map: Map[String, Any] ): A = {
		val clazz = t.toClass[A]
		val a = clazz.newInstance
		for ( (name, value) <- map ) clazz.declaredField( name ) match {
			case None => throw new UnmarshalException( "field " + name + " is not declared in " + clazz )
			case Some( field ) => field.set( a, unmarshalValue( field.getGenericType.resolveWith( t ), value ) )
		}
		a
	}

	def unmarshalValue( t: Type, value: Any ): Any = {
		def _unmarshal( t: Type, filter: Class[_] => Boolean ) = marshallers.find( t => filter( t._1 ) ) match {
			case Some( (_, m) ) => m.unmarshal( value, t )
			case None if value.isInstanceOf[Map[_, _]] => unmarshal[AnyRef]( t, value.asInstanceOf[Map[String, Any]] )
			case _ => throw new UnmarshalException( "could not unmarshal " + value + " as " + t )
		}

		if ( value == null ) null else t match {
			case clazz: Class[_] => _unmarshal( clazz, t => clazz.isAssignableFrom( t ) )
			case pt: ParameterizedType => _unmarshal( pt, t => pt.getRawType.asInstanceOf[Class[_]].isAssignableFrom( t ) )
		}
	}

	val dangerous = Map( '\\' -> "\\\\", '"' -> "\\\"", '\n' -> "\\n", '\r' -> "\\r", '\t' -> "\\t" )

	def quote( value: String ): String = value.foldLeft( "" )( (seed, c) => dangerous.get( c ) match {
		case Some( r ) => seed + r
		case None => seed + c
	} )

	private var marshallers: Map[Class[_], Marshaller] = Map(
		classOf[Int] -> new NumericMarshaller( _.toInt ),
		classOf[java.lang.Integer] -> new NumericMarshaller( _.toInt ),
		classOf[Long] -> new NumericMarshaller( _.toLong ),
		classOf[java.lang.Long] -> new NumericMarshaller( _.toLong ),
		classOf[Short] -> new NumericMarshaller( _.toShort ),
		classOf[java.lang.Short] -> new NumericMarshaller( _.toShort ),
		classOf[Byte] -> new NumericMarshaller( _.toByte ),
		classOf[java.lang.Byte] -> new NumericMarshaller( _.toByte ),
		classOf[Float] -> new NumericMarshaller( _.toFloat ),
		classOf[java.lang.Float] -> new NumericMarshaller( _.toFloat ),
		classOf[Double] -> new NumericMarshaller( d => d ),
		classOf[java.lang.Double] -> new NumericMarshaller( d => d ),
		classOf[Boolean] -> new ParametricMarshaller( unmarshaller = b => b ),
		classOf[java.lang.Boolean] -> new ParametricMarshaller( unmarshaller = b => b ),
		classOf[String] -> new ParametricMarshaller( s => "\"" + quote( s.toString ) + "\"", s => s ),
		classOf[List[_]] -> ListMarshaller
		)

	def registerMarshaller( c: Class[_], m: Marshaller ) = synchronized {
		marshallers += c -> m
	}

	trait Marshaller {
		def marshal( tab: String, a: Any, t: Type ): String

		def unmarshal( s: Any, t: Type ): Any
	}

	class ParametricMarshaller( marshaller: ( Any => String ) = ( _.toString ), unmarshaller: ( Any => Any ) ) extends Marshaller {
		def marshal( tab: String, a: Any, t: Type ) = marshaller( a )

		def unmarshal( s: Any, t: Type ) = unmarshaller( s )
	}

	class NumericMarshaller( unmarshaller: ( Double => Any ) ) extends ParametricMarshaller( unmarshaller = a => unmarshaller( a.asInstanceOf[Double] ) )

	object ListMarshaller extends Marshaller {
		def unmarshal( value: Any, pt: Type ) = value.asInstanceOf[List[_]].map( unmarshalValue( pt.asInstanceOf[ParameterizedType].getActualTypeArguments()( 0 ), _ ) )

		def marshal( tab: String, a: Any, pt: Type ) = "[\n" + a.asInstanceOf[List[_]].map(
			e => tab + marshalValue( tab + "\t", pt.asInstanceOf[ParameterizedType].getActualTypeArguments()( 0 ), e )
			).mkString( ",\n" ) + "\n" + tab.substring( 1 ) + "]"
	}
}


