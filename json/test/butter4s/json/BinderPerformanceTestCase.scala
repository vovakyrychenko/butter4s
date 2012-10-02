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
package butter4s.json

import annotation.{Element, Attribute}
import scala.annotation.target.field
import javax.xml.bind.annotation.{XmlElement, XmlAttribute}
import butter4s.benchmark._
import org.junit.Test

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */
class BinderPerformanceTestCase {
	@Test def performance = benchmark( "marshal/unmarshal", 500 ) {
		Binder.marshal( Binder.unmarshal[Year]( YearJson.json ) )
	}
}

object YearJson {
	val json = """{"year": 2020,"months": [{"name":"January", "number":1, "days": [{"date":1,"kind":"W"},{"date":2,"kind":"W"},{"date":3,"kind":"W"},{"date":4,"kind":"O"},{"date":5,"kind":"O"},{"date":6,"kind":"W"},{"date":7,"kind":"W"},{"date":8,"kind":"W"},{"date":9,"kind":"W"},{"date":10,"kind":"W"},{"date":11,"kind":"O"},{"date":12,"kind":"O"},{"date":13,"kind":"W"},{"date":14,"kind":"W"},{"date":15,"kind":"W"},{"date":16,"kind":"W"},{"date":17,"kind":"W"},{"date":18,"kind":"O"},{"date":19,"kind":"O"},{"date":20,"kind":"W"},{"date":21,"kind":"W"},{"date":22,"kind":"W"},{"date":23,"kind":"W"},{"date":24,"kind":"W"},{"date":25,"kind":"O"},{"date":26,"kind":"O"},{"date":27,"kind":"W"},{"date":28,"kind":"W"},{"date":29,"kind":"W"},{"date":30,"kind":"W"},{"date":31,"kind":"W"}]},{"name":"February", "number":2, "days": [{"date":1,"kind":"O"},{"date":2,"kind":"O"},{"date":3,"kind":"W"},{"date":4,"kind":"W"},{"date":5,"kind":"W"},{"date":6,"kind":"W"},{"date":7,"kind":"W"},{"date":8,"kind":"O"},{"date":9,"kind":"O"},{"date":10,"kind":"W"},{"date":11,"kind":"W"},{"date":12,"kind":"W"},{"date":13,"kind":"W"},{"date":14,"kind":"W"},{"date":15,"kind":"O"},{"date":16,"kind":"O"},{"date":17,"kind":"W"},{"date":18,"kind":"W"},{"date":19,"kind":"W"},{"date":20,"kind":"W"},{"date":21,"kind":"W"},{"date":22,"kind":"O"},{"date":23,"kind":"O"},{"date":24,"kind":"W"},{"date":25,"kind":"W"},{"date":26,"kind":"W"},{"date":27,"kind":"W"},{"date":28,"kind":"W"},{"date":29,"kind":"O"}]},{"name":"March", "number":3, "days": [{"date":1,"kind":"O"},{"date":2,"kind":"W"},{"date":3,"kind":"W"},{"date":4,"kind":"W"},{"date":5,"kind":"W"},{"date":6,"kind":"W"},{"date":7,"kind":"O"},{"date":8,"kind":"O"},{"date":9,"kind":"W"},{"date":10,"kind":"W"},{"date":11,"kind":"W"},{"date":12,"kind":"W"},{"date":13,"kind":"W"},{"date":14,"kind":"O"},{"date":15,"kind":"O"},{"date":16,"kind":"W"},{"date":17,"kind":"W"},{"date":18,"kind":"W"},{"date":19,"kind":"W"},{"date":20,"kind":"W"},{"date":21,"kind":"O"},{"date":22,"kind":"O"},{"date":23,"kind":"W"},{"date":24,"kind":"W"},{"date":25,"kind":"W"},{"date":26,"kind":"W"},{"date":27,"kind":"W"},{"date":28,"kind":"O"},{"date":29,"kind":"O"},{"date":30,"kind":"W"},{"date":31,"kind":"W"}]},{"name":"April", "number":4, "days": [{"date":1,"kind":"W"},{"date":2,"kind":"W"},{"date":3,"kind":"W"},{"date":4,"kind":"O"},{"date":5,"kind":"O"},{"date":6,"kind":"W"},{"date":7,"kind":"W"},{"date":8,"kind":"W"},{"date":9,"kind":"W"},{"date":10,"kind":"W"},{"date":11,"kind":"O"},{"date":12,"kind":"O"},{"date":13,"kind":"W"},{"date":14,"kind":"W"},{"date":15,"kind":"W"},{"date":16,"kind":"W"},{"date":17,"kind":"W"},{"date":18,"kind":"O"},{"date":19,"kind":"O"},{"date":20,"kind":"W"},{"date":21,"kind":"W"},{"date":22,"kind":"W"},{"date":23,"kind":"W"},{"date":24,"kind":"W"},{"date":25,"kind":"O"},{"date":26,"kind":"O"},{"date":27,"kind":"W"},{"date":28,"kind":"W"},{"date":29,"kind":"W"},{"date":30,"kind":"W"}]},{"name":"May", "number":5, "days": [{"date":1,"kind":"W"},{"date":2,"kind":"O"},{"date":3,"kind":"O"},{"date":4,"kind":"W"},{"date":5,"kind":"W"},{"date":6,"kind":"W"},{"date":7,"kind":"W"},{"date":8,"kind":"W"},{"date":9,"kind":"O"},{"date":10,"kind":"O"},{"date":11,"kind":"W"},{"date":12,"kind":"W"},{"date":13,"kind":"W"},{"date":14,"kind":"W"},{"date":15,"kind":"W"},{"date":16,"kind":"O"},{"date":17,"kind":"O"},{"date":18,"kind":"W"},{"date":19,"kind":"W"},{"date":20,"kind":"W"},{"date":21,"kind":"W"},{"date":22,"kind":"W"},{"date":23,"kind":"O"},{"date":24,"kind":"O"},{"date":25,"kind":"W"},{"date":26,"kind":"W"},{"date":27,"kind":"W"},{"date":28,"kind":"W"},{"date":29,"kind":"W"},{"date":30,"kind":"O"},{"date":31,"kind":"O"}]},{"name":"June", "number":6, "days": [{"date":1,"kind":"W"},{"date":2,"kind":"W"},{"date":3,"kind":"W"},{"date":4,"kind":"W"},{"date":5,"kind":"W"},{"date":6,"kind":"O"},{"date":7,"kind":"O"},{"date":8,"kind":"W"},{"date":9,"kind":"W"},{"date":10,"kind":"W"},{"date":11,"kind":"W"},{"date":12,"kind":"W"},{"date":13,"kind":"O"},{"date":14,"kind":"O"},{"date":15,"kind":"W"},{"date":16,"kind":"W"},{"date":17,"kind":"W"},{"date":18,"kind":"W"},{"date":19,"kind":"W"},{"date":20,"kind":"O"},{"date":21,"kind":"O"},{"date":22,"kind":"W"},{"date":23,"kind":"W"},{"date":24,"kind":"W"},{"date":25,"kind":"W"},{"date":26,"kind":"W"},{"date":27,"kind":"O"},{"date":28,"kind":"O"},{"date":29,"kind":"W"},{"date":30,"kind":"W"}]},{"name":"July", "number":7, "days": [{"date":1,"kind":"W"},{"date":2,"kind":"W"},{"date":3,"kind":"W"},{"date":4,"kind":"O"},{"date":5,"kind":"O"},{"date":6,"kind":"W"},{"date":7,"kind":"W"},{"date":8,"kind":"W"},{"date":9,"kind":"W"},{"date":10,"kind":"W"},{"date":11,"kind":"O"},{"date":12,"kind":"O"},{"date":13,"kind":"W"},{"date":14,"kind":"W"},{"date":15,"kind":"W"},{"date":16,"kind":"W"},{"date":17,"kind":"W"},{"date":18,"kind":"O"},{"date":19,"kind":"O"},{"date":20,"kind":"W"},{"date":21,"kind":"W"},{"date":22,"kind":"W"},{"date":23,"kind":"W"},{"date":24,"kind":"W"},{"date":25,"kind":"O"},{"date":26,"kind":"O"},{"date":27,"kind":"W"},{"date":28,"kind":"W"},{"date":29,"kind":"W"},{"date":30,"kind":"W"},{"date":31,"kind":"W"}]},{"name":"August", "number":8, "days": [{"date":1,"kind":"O"},{"date":2,"kind":"O"},{"date":3,"kind":"W"},{"date":4,"kind":"W"},{"date":5,"kind":"W"},{"date":6,"kind":"W"},{"date":7,"kind":"W"},{"date":8,"kind":"O"},{"date":9,"kind":"O"},{"date":10,"kind":"W"},{"date":11,"kind":"W"},{"date":12,"kind":"W"},{"date":13,"kind":"W"},{"date":14,"kind":"W"},{"date":15,"kind":"O"},{"date":16,"kind":"O"},{"date":17,"kind":"W"},{"date":18,"kind":"W"},{"date":19,"kind":"W"},{"date":20,"kind":"W"},{"date":21,"kind":"W"},{"date":22,"kind":"O"},{"date":23,"kind":"O"},{"date":24,"kind":"W"},{"date":25,"kind":"W"},{"date":26,"kind":"W"},{"date":27,"kind":"W"},{"date":28,"kind":"W"},{"date":29,"kind":"O"},{"date":30,"kind":"O"},{"date":31,"kind":"W"}]},{"name":"September", "number":9, "days": [{"date":1,"kind":"W"},{"date":2,"kind":"W"},{"date":3,"kind":"W"},{"date":4,"kind":"W"},{"date":5,"kind":"O"},{"date":6,"kind":"O"},{"date":7,"kind":"W"},{"date":8,"kind":"W"},{"date":9,"kind":"W"},{"date":10,"kind":"W"},{"date":11,"kind":"W"},{"date":12,"kind":"O"},{"date":13,"kind":"O"},{"date":14,"kind":"W"},{"date":15,"kind":"W"},{"date":16,"kind":"W"},{"date":17,"kind":"W"},{"date":18,"kind":"W"},{"date":19,"kind":"O"},{"date":20,"kind":"O"},{"date":21,"kind":"W"},{"date":22,"kind":"W"},{"date":23,"kind":"W"},{"date":24,"kind":"W"},{"date":25,"kind":"W"},{"date":26,"kind":"O"},{"date":27,"kind":"O"},{"date":28,"kind":"W"},{"date":29,"kind":"W"},{"date":30,"kind":"W"}]},{"name":"October", "number":10, "days": [{"date":1,"kind":"W"},{"date":2,"kind":"W"},{"date":3,"kind":"O"},{"date":4,"kind":"O"},{"date":5,"kind":"W"},{"date":6,"kind":"W"},{"date":7,"kind":"W"},{"date":8,"kind":"W"},{"date":9,"kind":"W"},{"date":10,"kind":"O"},{"date":11,"kind":"O"},{"date":12,"kind":"W"},{"date":13,"kind":"W"},{"date":14,"kind":"W"},{"date":15,"kind":"W"},{"date":16,"kind":"W"},{"date":17,"kind":"O"},{"date":18,"kind":"O"},{"date":19,"kind":"W"},{"date":20,"kind":"W"},{"date":21,"kind":"W"},{"date":22,"kind":"W"},{"date":23,"kind":"W"},{"date":24,"kind":"O"},{"date":25,"kind":"O"},{"date":26,"kind":"W"},{"date":27,"kind":"W"},{"date":28,"kind":"W"},{"date":29,"kind":"W"},{"date":30,"kind":"W"},{"date":31,"kind":"O"}]},{"name":"November", "number":11, "days": [{"date":1,"kind":"O"},{"date":2,"kind":"W"},{"date":3,"kind":"W"},{"date":4,"kind":"W"},{"date":5,"kind":"W"},{"date":6,"kind":"W"},{"date":7,"kind":"O"},{"date":8,"kind":"O"},{"date":9,"kind":"W"},{"date":10,"kind":"W"},{"date":11,"kind":"W"},{"date":12,"kind":"W"},{"date":13,"kind":"W"},{"date":14,"kind":"O"},{"date":15,"kind":"O"},{"date":16,"kind":"W"},{"date":17,"kind":"W"},{"date":18,"kind":"W"},{"date":19,"kind":"W"},{"date":20,"kind":"W"},{"date":21,"kind":"O"},{"date":22,"kind":"O"},{"date":23,"kind":"W"},{"date":24,"kind":"W"},{"date":25,"kind":"W"},{"date":26,"kind":"W"},{"date":27,"kind":"W"},{"date":28,"kind":"O"},{"date":29,"kind":"O"},{"date":30,"kind":"W"}]},{"name":"December", "number":12, "days": [{"date":1,"kind":"W"},{"date":2,"kind":"W"},{"date":3,"kind":"W"},{"date":4,"kind":"W"},{"date":5,"kind":"O"},{"date":6,"kind":"O"},{"date":7,"kind":"W"},{"date":8,"kind":"W"},{"date":9,"kind":"W"},{"date":10,"kind":"W"},{"date":11,"kind":"W"},{"date":12,"kind":"O"},{"date":13,"kind":"O"},{"date":14,"kind":"W"},{"date":15,"kind":"W"},{"date":16,"kind":"W"},{"date":17,"kind":"W"},{"date":18,"kind":"W"},{"date":19,"kind":"O"},{"date":20,"kind":"O"},{"date":21,"kind":"W"},{"date":22,"kind":"W"},{"date":23,"kind":"W"},{"date":24,"kind":"W"},{"date":25,"kind":"W"},{"date":26,"kind":"O"},{"date":27,"kind":"O"},{"date":28,"kind":"W"},{"date":29,"kind":"W"},{"date":30,"kind":"W"},{"date":31,"kind":"W"}]}]}"""

}
class Year {
	@( Attribute@field ) var year: Int = _
	@( Element@field ) var months = List[Month]()
}

class Month {
	@( Attribute@field ) var name: String = _
	@( Attribute@field ) var number: Int = _
	@( Element@field ) var days = List[Day]()
}

class Day {
	@( Attribute@field ) var date: Int = _
	@( Attribute@field ) var kind: String = _
}
