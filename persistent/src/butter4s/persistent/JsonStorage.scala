package butter4s.persistent


import butter4s.fs.{Directory, File}
import butter4s.json.Binder
import scala.reflect.Manifest
import butter4s.lang.reflect._
import butter4s.lang._

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */

trait JsonStorage[ T <: AnyRef ] {
	val location: String

	def find(id: String)(implicit m: Manifest[ T ]): Option[ T ] = findRaw(id).flatMap(Binder.unmarshal[ T ](_))

	def findRaw(id: String): Option[ String ] = synchronized{
		val file = new File(location + "/" + id + ".json")
		if( file.exists ) Some(file.read[ String ])
		else None
	}

	def list(implicit m: Manifest[ T ]): List[ T ] = synchronized{
		new Directory(location).filter(_.name.endsWith(".json")).map(f => Binder.unmarshal[ T ](f.asInstanceOf[ File ].read[ String ]).get)
	}

	def listRaw: List[ String ] = synchronized{
		new Directory(location).filter(_.name.endsWith(".json")).map(_.asInstanceOf[ File ].read[ String ])
	}

	def store(t: T) = synchronized{
		new File(path(t)).write(Binder.marshal(t))
	}

	def isolated(localPath: String) = new JsonStorage[ T ] {
		val location = JsonStorage.this.location + "/" + localPath
	}

	private def path(t: T) = location + "/" + identify(t) + ".json"

	def identify(t: T) = t.typeOf.as[ raw.RefType ].fields.find(_.annotatedWith[ Key ]) match {
		case Some(f) => f.accessible[ Any ](f.get(t))
		case None => throw new StorageException(t.typeOf + " has no field annotated with @Key")
	}

	def updateAll(ts: Seq[ T ]) = {
		new Directory(location).clear
		for( t <- ts ) store(t)
	}

	def delete(t: T) = synchronized{
		new File(path(t)).delete
	}
}


class StorageException(msg: String) extends RuntimeException(msg)
