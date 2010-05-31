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

import org.junit.Test
import scala.annotation.target.field
import javax.xml.bind.annotation.{XmlAttribute, XmlElement}

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */
class JSONBindTestCase {
	@Test def bind = assertBind( Bean( "x", 10, Bean2( "y", 15, List( 1, 2, 3 ) ) ) )

	@Test def bindListList = assertBind( ListBean( List( List( 1, 2, 3 ), List( 4, 5, 6 ) ) ) )

	@Test def bindGenericObject = assertBind( BeanGB( BeanGeneric( List( 1, 2, 3 ) ) ) )

	@Test def bindDeepGeneric = assertBind( BeanGB2( BeanGeneric( List( BeanGeneric( List( 1, 2 ) ), BeanGeneric( List( 3, 4 ) ) ) ),
		BeanGeneric( List( BeanGeneric( List( 5, 6 ) ), BeanGeneric( List( 7, 8 ) ) ) ) ) )

	def assertBind[A <: AnyRef : Manifest]( source: A ) = {
		val json = JSONBind.marshal( source )
		println( json )
		val result = JSONBind.unmarshal[A]( json )
		println( result )
		assert( source == result )
	}

}

case class Bean( @( XmlElement@field ) var str: String, @( XmlAttribute@field ) var i: Int, @( XmlElement@field ) var sb2: Bean2 ) {
	def this() = this ( null, 0, null )
}

case class Bean2( @( XmlElement@field ) var s2: String, @( XmlAttribute@field ) var i2: Int, @( XmlElement@field ) list: List[Int] = List() ) {
	def this() = this ( null, 0 )
}

case class ListBean( @( XmlElement@field ) var l: List[List[Int]] ) {
	def this() = this ( List() )
}

case class BeanGeneric[A]( @( XmlElement@field ) var a: A ) {
	def this() = this ( null.asInstanceOf[A] )
}

case class BeanGB( @( XmlElement@field ) var bg: BeanGeneric[List[Int]] ) {
	def this() = this ( null )
}

case class BeanGB2( @( XmlElement@field ) var bg: BeanGeneric[List[BeanGeneric[List[Int]]]],
                    @( XmlElement@field ) var bg2: BeanGeneric[List[BeanGeneric[List[Int]]]] ) {
	def this() = this ( null, null )
}