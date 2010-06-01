package butter4s.persistent


import butter4s.fs.{Directory, File}
import butter4s.bind.json.JSONBind
import scala.reflect.Manifest
import butter4s.reflect._
import butter4s.lang._

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */

trait JSONStorage[T <: AnyRef] {
	val location: String

	def find( id: String )( implicit m: Manifest[T] ): Option[T] = find( "", id )

	def find( path: String, id: String )( implicit m: Manifest[T] ): Option[T] =
		findRaw( path, id ).map( JSONBind.unmarshal[T]( _ ) )

	def findRaw( path: String, id: String ): Option[String] = synchronized {
		val file = new File( location + "/" + path + "/" + id + ".json" )
		if ( file.exists ) Some( file.read )
		else None
	}

	def list( implicit m: Manifest[T] ): Array[T] = synchronized {
		new Directory( location ).list( _.name.endsWith( ".json" ) ).map( f => JSONBind.unmarshal[T]( f.asInstanceOf[File].read ) )
	}

	def listRaw: Array[String] = synchronized {
		new Directory( location ).list( _.name.endsWith( ".json" ) ).map( _.asInstanceOf[File].read[String] )
	}

	def store( path: String, obj: T ) = synchronized {
		val id = obj.getClass.annotatedField[Key].get.get( obj )
		new File( location + "/" + path + "/" + id + ".json" ).write( JSONBind.marshal( obj ) )
	}
}

