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
package butter4s.lang.reflect

import org.junit.Test
import java.lang.annotation.RetentionPolicy
import org.scalatest.junit.{ShouldMatchersForJUnit, AssertionsForJUnit}
import com.weiglewilczek.slf4s.Logging

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */

class ReflectTestCase extends AssertionsForJUnit with ShouldMatchersForJUnit with Logging {
  @Test def parameterizedTypes() {
    val t = typeOf[List[Map[RetentionPolicy, List[Int]]]]
    t.toString should equal("ClassType(List[InterfaceType(Map[EnumType(RetentionPolicy),ClassType(List[PrimitiveType(int)])])])")
    logger.info(t.toString)
    logger.info(typeOf[Array[Int]].toString)
  }

  @Test def typeManifest() {
    val t = new parameterized.TypeManifest[List[Map[RetentionPolicy, List[Integer]]]] {}.asParameterizedType
    t.toString should equal("ClassType(List[InterfaceType(Map[EnumType(RetentionPolicy),ClassType(List[ClassType(Integer)])])])")
    logger.info(t.toString)

    val t2 = new parameterized.TypeManifest[String] {}.asParameterizedType
    t2.toString should equal("ClassType(String)")
    logger.info(t2.toString)
  }


  @Test def equals() {
    typeOf[List[String]] == typeOf[List[String]] should equal(true)
    typeOf[List[String]] != typeOf[List[Int]] should equal(true)
    typeOf[List[String]].rawType == typeOf[List[Int]].rawType should equal(true)
  }

  @Test def assignableFrom() {
    typeOf[AnyRef].rawType <:< typeOf[String].rawType should equal(true)
    typeOf[String].rawType <:< typeOf[String].rawType should equal(true)
    typeOf[String].rawType <:< typeOf[AnyRef].rawType should equal(false)
    typeOf[Enum[_]].rawType <:< typeOf[RetentionPolicy].rawType should equal(true)
  }

  @Test def fields() {
    val ct = typeOf[X[String]].as[parameterized.ClassType]
    ct.fields.map(_.raw.name) should equal(List("x", "str", "t"))

    val f = ct.fields.find(_.raw.name == "t").get

    val dt: parameterized.ClassType[X[String]] = f.ownerType

    f.actualType should equal(typeOf[String])

    val x = new X[String]
    f.raw.accessible {
      f.raw.set(x, "aaa");
      f.raw.get(x)
    } should equal("aaa")
  }

  @Test def enum() {
    typeOf[RetentionPolicy].as[parameterized.EnumType].rawType.values should equal(RetentionPolicy.values.toList)
  }
}

class X[T <: AnyRef] {
  var x = 1;
  var str = "aaa";
  var t: T = _
}