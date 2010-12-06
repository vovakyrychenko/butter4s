package butter4s.persistent


import butter4s.fs.{Directory, File}
import butter4s.json.Binder
import scala.reflect.Manifest
import butter4s.lang.reflect._
import butter4s.lang._

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */

trait JSONStorage[T <: AnyRef] {
	val location: String

	def find( id: String )( implicit m: Manifest[T] ): Option[T] = findRaw( id ).flatMap( Binder.unmarshal[T]( _ ) )

	def findRaw( id: String ): Option[String] = synchronized {
		val file = new File( location + "/" + id + ".json" )
		if ( file.exists ) Some( file.read[String] )
		else None
	}

	def list( implicit m: Manifest[T] ): List[T] = synchronized {
		new Directory( location ).filter( _.name.endsWith( ".json" ) ).map( f => Binder.unmarshal[T]( f.asInstanceOf[File].read[String] ).get )
	}

	def listRaw: List[String] = synchronized {
		new Directory( location ).filter( _.name.endsWith( ".json" ) ).map( _.asInstanceOf[File].read[String] )
	}

	def store( obj: T ) = synchronized {
		obj.typeOf.as[raw.RefType].fields.find( _.annotatedWith[Key] ) match {
			case Some( f ) => new File( location + "/" + f.get( obj ) + ".json" ).write( Binder.marshal( obj ) )
			case None => throw new StorageException( obj.typeOf + " has no field annotated with @Key" )
		}
	}

	def isolated( path: String ) = new JSONStorage[T] {
		val location = JSONStorage.this.location + "/" + path
	}
}


class StorageException( msg: String ) extends RuntimeException( msg ) 
