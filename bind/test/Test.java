/*
 * The MIT License
 *
 *  Copyright (c) 2010 Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 *
 *  Permission is hereby granted, free of charge, to any person obtaining a copy
 *  of this software and associated documentation files (the "Software"), to deal
 *  in the Software without restriction, including without limitation the rights
 *  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 *  copies of the Software, and to permit persons to whom the Software is
 *  furnished to do so, subject to the following conditions:
 *
 *  The above copyright notice and this permission notice shall be included in
 *  all copies or substantial portions of the Software.
 *
 *  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 *  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 *  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 *  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 *  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 *  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 *  THE SOFTWARE.
 */

import java.lang.reflect.ParameterizedType;
import java.util.Arrays;
import java.util.List;

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 */
public class Test {
	int i;
	String s;
	Object o;
	List<String> l1;
	List<List<String>> l2;

	public static void main( String[] args ) throws NoSuchFieldException {
		for( String name : Arrays.asList( "i", "s", "o", "l1", "l2" ) )
			System.out.println( name + ": " + Test.class.getDeclaredField( name ).getGenericType().getClass() );

		System.out.println( "l2 inspect : " + ( ( ParameterizedType ) Test.class.getDeclaredField( "l2" ).getGenericType() ).getRawType() );
		System.out.println( "l2 inspect : " + ( ( ParameterizedType ) Test.class.getDeclaredField( "l2" ).getGenericType() ).getActualTypeArguments()[0] );
	}
}
