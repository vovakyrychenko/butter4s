package butter4s.io

import butter4s.fs.File
import java.io.BufferedOutputStream

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */

class FileOutputStream( path: String, append: Boolean ) extends BufferedOutputStream( {new File( path ).parent.create; new java.io.FileOutputStream( path )} ) {
	def this( path: String ) = this ( path, false )
}