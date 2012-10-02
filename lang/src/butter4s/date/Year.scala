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
package butter4s.date

import java.util.{GregorianCalendar, Locale, Calendar}
import java.util.Calendar._
/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */
case class Year( year: Int ) extends Seq[Month] {
	private[date] val calendar = new GregorianCalendar
	calendar.set( YEAR, year )
	private lazy val monthes = for ( m <- calendar.getActualMinimum( MONTH ) to calendar.getActualMaximum( MONTH ) ) yield Month( this, m )

	def iterator = monthes.toIterator

	def length = monthes.length

	def apply( idx: Int ) = monthes( idx )

	override def toString() = "Year(" + year + ")"
}

case class Month( year: Year, number: Int ) extends Seq[Day] {
	private[date] val calendar = year.calendar.clone.asInstanceOf[Calendar];
	calendar.set( MONTH, number )
	lazy val name:String = calendar.getDisplayName( MONTH, LONG, Locale.getDefault )
	private lazy val days = for ( d <- calendar.getActualMinimum( DATE ) to calendar.getActualMaximum( DATE ) ) yield Day( this, d )


	def iterator = days.toIterator

	def length = days.length

	def apply( idx: Int ) = days( idx )

	override def toString = name
}

case class Day( month: Month, number: Int ) {
	private[date] val calendar = month.calendar.clone.asInstanceOf[Calendar];
	calendar.set( DATE, number )
	lazy val ofWeek = calendar.get( DAY_OF_WEEK ) match {
		case MONDAY => Monday
		case TUESDAY => Tuesday
		case WEDNESDAY => Wednesday
		case THURSDAY => Thursday
		case FRIDAY => Friday
		case SATURDAY => Saturday
		case SUNDAY => Sunday
	}

	override def toString = ofWeek + "(" + month + "," + number + ")"
}

sealed trait DayOfWeek
case object Monday extends DayOfWeek
case object Tuesday extends DayOfWeek
case object Wednesday extends DayOfWeek
case object Thursday extends DayOfWeek
case object Friday extends DayOfWeek
case object Saturday extends DayOfWeek
case object Sunday extends DayOfWeek

