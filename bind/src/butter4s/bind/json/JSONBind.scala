/**************************************************************************
 *
 * Copyright (c) Adstream Pty Ltd
 *
 * This software is the confidential and proprietary information of
 * Adstream Pty Ltd ("Confidential Information").  You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Adstream Pty Ltd.
 *
 ************************************************************************
 */

package butter4s.bind.json


import butter4s.bind._
import butter4s.reflect._

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */
object JSONBind {
	def marshal( a: AnyRef ): String = if ( a == null ) "null" else {
		require( a.getClass.annotatedWith[Bindable], "class should be @Bindable" )
		marshalObject( "\t", a )
	}

	private def marshalObject( tab: String, a: AnyRef ): String = {
		val fields: List[Field] = a.getClass.declaredFields.filter( field => field.annotatedWith[Element] || field.annotatedWith[Attribute] )
		"{\n" +
				fields.map( field => tab + "\"" + field.name + "\": " + marshalValue( tab + "\t", field.get( a ) ) ).mkString( ",\n" ) +
				"\n" + tab.substring( 1 ) + "}"
	}

	private def marshalValue( tab: String, value: Any ): String = value match {
		case value: Int => value.toString
		case value: Byte => value.toString
		case value: Long => value.toString
		case value: Short => value.toString
		case value: Float => value.toString
		case value: Double => value.toString
		case value: String => "\"" + value + "\""
		case value: List[_] => "[\n" + value.map( tab + marshalValue( tab + "\t", _ ) ).mkString( ",\n" ) + "\n" + tab.substring( 1 ) + "]"
		case value: AnyRef => marshalObject( tab, value )
		case null => "null"
	}

	def unmarshal[A <: AnyRef]( input: String )( implicit m: Manifest[A] ): A = JSON.parse( input ) match {
		case None => throw new BindException( "could not parse", input )
		case Some( map ) => unmarshal[A]( m.erasure.asInstanceOf[Class[A]], map.asInstanceOf[Map[String, Any]] )
	}

	private def unmarshal[A <: AnyRef]( clazz: Class[A], map: Map[String, Any] ): A = {
		val a: A = clazz.newInstance
		for ( (name, value) <- map ) clazz.declaredField( name ) match {
			case None => throw new BindException( "field " + name + " is not declared in " + clazz )
			case Some( field ) => field.set( a, unmarshalValue( field.getType, field.annotation[ListElementType], value ) )
		}
		a
	}

	private def unmarshalValue( clazz: Class[_], annotation: => Option[ListElementType], value: Any ): Any = {
		if ( clazz.assignableFrom[Int] ) value.asInstanceOf[Double].toInt
		else if ( clazz.assignableFrom[Long] ) value.asInstanceOf[Double].toLong
		else if ( clazz.assignableFrom[Short] ) value.asInstanceOf[Double].toShort
		else if ( clazz.assignableFrom[Byte] ) value.asInstanceOf[Double].toByte
		else if ( clazz.assignableFrom[Float] ) value.asInstanceOf[Double].toFloat
		else if ( clazz.assignableFrom[Double] ) value
		else if ( clazz.assignableFrom[Boolean] ) value
		else if ( clazz.assignableFrom[String] ) value
		else if ( clazz.assignableFrom[List[_]] ) annotation match {
			case None => throw new BindException( "missing @ListElementType" )
			case Some( a: ListElementType ) => value.asInstanceOf[List[_]].map( unmarshalValue( a.value, None, _ ) )
		}
		else if ( value.isInstanceOf[Map[_, _]] ) unmarshal[AnyRef]( clazz.asInstanceOf[Class[AnyRef]], value.asInstanceOf[Map[String, Any]] )
		else throw new BindException( "could not unmarshal " + value )
	}

}


