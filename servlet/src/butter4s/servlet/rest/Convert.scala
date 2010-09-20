package butter4s.servlet.rest

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */

object Convert {
	private val converters: Map[Class[_], ( String => Any )] = Map(
		classOf[Int] -> {_.toInt},
		classOf[java.lang.Integer] -> {_.toInt},
		classOf[Long] -> {_.toLong},
		classOf[java.lang.Long] -> ( _.toLong ),
		classOf[Short] -> ( _.toShort ),
		classOf[java.lang.Short] -> ( _.toShort ),
		classOf[Byte] -> ( _.toByte ),
		classOf[java.lang.Byte] -> ( _.toByte ),
		classOf[Float] -> ( _.toFloat ),
		classOf[java.lang.Float] -> ( _.toFloat ),
		classOf[Double] -> ( _.toDouble ),
		classOf[java.lang.Double] -> {_.toDouble},
		classOf[Boolean] -> {_.toBoolean},
		classOf[java.lang.Boolean] -> {_.toBoolean},
		classOf[String] -> {s => s}
		)

	def to( value: String, clazz: Class[_] ) = converters( clazz )( value )
}