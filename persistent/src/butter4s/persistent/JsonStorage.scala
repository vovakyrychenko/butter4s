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
package butter4s.persistent


import butter4s.fs.{Directory, File}
import butter4s.json.Binder
import scala.reflect.Manifest
import butter4s.lang.reflect._
import butter4s.lang._

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */

trait CanBeIsolated[S <: JsonStorage[_]] {
	def isolated(isolationPath: String): S
}

trait BasicIsolation[T <: AnyRef] extends CanBeIsolated[JsonStorage[T]] {
	val location: String

	def isolated(isolationPath: String) = new JsonStorage[T] {
		val location = BasicIsolation.this.location + "/" + isolationPath
	}
}

trait JsonStorage[T <: AnyRef] {
	val location: String

	def get(id: String)(implicit m: Manifest[T]): Option[T] = getRaw(id).flatMap(Binder.unmarshal[T](_))

	def getRaw(id: String): Option[String] = synchronized {
		val file = new File(location + "/" + id + ".json")
		if( file.exists ) Some(file.read[String])
		else None
	}

	def list(implicit m: Manifest[T]): List[T] = synchronized {
		new Directory(location).filter(_.name.endsWith(".json")).map(f => Binder.unmarshal[T](f.asInstanceOf[File].read[String]).get)
	}

	def listRaw: List[String] = synchronized {
		new Directory(location).filter(_.name.endsWith(".json")).map(_.asInstanceOf[File].read[String])
	}

	def store(t: T) = synchronized {
		new File(path(t)).write(Binder.marshal(t))
	}

	private def path(t: T) = location + "/" + identify(t) + ".json"

	def identify(t: T) = t.typeOf.as[raw.RefType].fields.find(_.annotatedWith[Key]) match {
		case Some(f) => f.accessible[Any](f.get(t))
		case None => throw new StorageException(t.typeOf + " has no field annotated with @Key")
	}

	def updateAll(ts: Seq[T]) = {
		new Directory(location).clear()
		for( t <- ts ) store(t)
	}

	def delete(t: T) = synchronized {
		new File(path(t)).delete()
	}
}


class StorageException(msg: String) extends RuntimeException(msg)
