import javax.xml.bind.annotation.{XmlAttribute, XmlRootElement}

/**************************************************************************
 *
 * Copyright (c) Adstream Pty Ltd
 *
 * This software is the confidential and proprietary information of
 * Adstream Pty Ltd ("Confidential Information").  You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Adstream Pty Ltd.
 *
 ************************************************************************
 */


/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */
@XmlRootElement
class X( @XmlAttribute var a: String ) {
	@XmlAttribute var b: String = _;
}

object Y {
	def main(args:Array[String]) {
		println(new X("aaa").a)
	}
}