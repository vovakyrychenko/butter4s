package butter4s.fs

import butter4s.io._
import butter4s.lang.reflect._
import collection.TraversableLike
import collection.mutable.{Builder, ListBuffer}
import java.io.{IOException, FileOutputStream}

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */

abstract class FsNode(_path: String) extends Ordered[ FsNode ] {
	val impl = new java.io.File(_path)

	def exists = impl.exists

	lazy val name = impl.getName

	lazy val path = impl.getPath

	def delete(): Unit

	override def toString = impl.toString

	def compare(that: FsNode) = this.name.compare(that.name)
}


class File(_path: String) extends FsNode(_path) {
	def read[ R ](implicit convert: FromArrayConversion[ R ]) = using(new FileInputStream(path)) {
		_.readAs[ R ]
	}

	def write[ P ](content: P)(implicit convert: ToArrayConversion[ P ]) = {
		parent.create
		using(new FileOutputStream(impl)) {
			_.write(content)
		}
	}

	def length = impl.length

	def delete() {
		if( !impl.delete ) throw new IOException("could not delete " + path)
	}

	lazy val parent = new Directory(impl.getParent)
}

class Directory(_path: String) extends FsNode(_path) with TraversableLike[ FsNode, List[ FsNode ] ] {
	private def items = impl.listFiles match {
		case null => Nil
		case xs => xs.view.map(file => if( file.isDirectory ) new Directory(file.getPath) else new File(file.getPath))
	}

	def apply[ N <: FsNode : Manifest ](name: String) = find(_.name == name).flatMap(_.ifInstanceOf[N])

	def create = impl.mkdirs

	def foreach[ U ](f: FsNode => U) {
		items.foreach(f)
	}

	def clear() {
		this.foreach(_.delete())
	}

	def delete() {
		this.foreach(_.delete())
		if( !impl.delete ) throw new IOException("could not delete " + path)
	}

	protected[ this ] def newBuilder: Builder[ FsNode, List[ FsNode ] ] = new ListBuffer[ FsNode ]

	def seq = this
}