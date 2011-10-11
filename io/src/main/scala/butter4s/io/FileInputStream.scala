package butter4s.io

import java.io.BufferedInputStream

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */

class FileInputStream( path: String ) extends BufferedInputStream( new java.io.FileInputStream( path ) ) 