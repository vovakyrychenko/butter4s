/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: JSON.scala 20028 2009-12-07 11:49:19Z cunei $


package butter4s.bind.json
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical._

/** 
 * This object provides a simple interface to the JSON parser class. The default conversion
 * for numerics is into a double. If you wish to override this behavior at the global level,
 * you can set the globalNumberParser property to your own (String => Any) function. If you only
 * want to override at the per-thread level then you can set the perThreadNumberParser property to your
 * function. For example:
 * 
 * <pre>
 * val myConversionFunc = {input : String => BigDecimal(input)}
 * 
 * // Global override
 * JSON.globalNumberParser = myConversionFunc
 * 
 * // Per-thread override
 * JSON.perThreadNumberParser = myConversionFunc
 * </pre>
 *
 *  @author Derek Chen-Becker <"java"+@+"chen-becker"+"."+"org">
 */
object JSON extends Parser {

  /**
   * Parse the given JSON string and return a list of elements. If the
   * string is a JSON object it will be a list of pairs. If it's a JSON 
   * array it will be be a list of individual elements.
   *
   * @param input the given JSON string.
   * @return      an optional list of of elements. 
   */
  def parse(input: String): Option[Any] =
    phrase(root)(new lexical.Scanner(input)) match {
      case Success(result, _) => Some(result)
      case _ => None
    }
  
 
  /**
   * The global (VM) default function for converting a string to a numeric value. 
   */
  def globalNumberParser_=(f: NumericParser) { defaultNumberParser = f }
  def globalNumberParser : NumericParser = defaultNumberParser
  
  /**
   * Defines the function used to convert a numeric string literal into a numeric format on a per-thread
   * basis. Use globalNumberParser for a global override
   */
   def perThreadNumberParser_=(f : NumericParser) { numberParser.set(f) }
   def perThreadNumberParser : NumericParser = numberParser.get()
}
