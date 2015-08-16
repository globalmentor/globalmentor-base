/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.globalmentor.text;

import com.globalmentor.java.Integers;

/**
 * General information and utilities for working with Unicode.
 * @author Garret Wilson
 */
public class Unicode {

	/**
	 * Creates a string representation of a given Unicode code point in the form "U+XXXX[XX]".
	 * @param codeValue The Unicode code point to represent.
	 * @return A string representation of the Unicode code point in the form "U+XXXX[XX]"
	 */
	public static String getCodePointString(final int codeValue) {
		final StringBuilder stringBuilder = new StringBuilder("U+"); //create a string buffer
		//append the code value, using six digits if needed
		stringBuilder.append(ASCII.toUpperCase(Integers.toHexString(codeValue, codeValue <= 0xFFFF ? 4 : 6)));
		return stringBuilder.toString(); //return the string we constructed		
	}

}