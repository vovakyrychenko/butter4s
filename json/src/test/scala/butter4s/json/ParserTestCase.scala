package butter4s.json

import butter4s.benchmark._
import org.junit.Test
import org.scalatest.junit.{ShouldMatchersForJUnit, AssertionsForJUnit}
import com.weiglewilczek.slf4s.Logging

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */

class ParserTestCase extends AssertionsForJUnit with ShouldMatchersForJUnit with Logging {
  @Test def parse() {
    Parser.parse("true") should equal(true)
    Parser.parse("false") should equal(false)
    Parser.parse("null") should equal(null)
    Parser.parse("123") should equal(123.0)
    Parser.parse("123.5") should equal(123.5)
    Parser.parse("\"str\"") should equal("str")
    Parser.parse("[]") should equal(List())
    Parser.parse("[1,2,3]") should equal(List(1.0, 2.0, 3.0))
    Parser.parse("[1,[1,2],3]") should equal(List(1.0, List(1.0, 2.0), 3.0))
    Parser.parse("{}") should equal(Map[String, Any]())
    Parser.parse("[{\"a\":[{}]}]") should equal(List(Map("a" -> List(Map()))))

    logger.info("" + Parser.parse(YearJson.json))
  }

  @Test def performance() {
    benchmark("parser", 500) {
      Parser.parse(YearJson.json)
    }
  }
}