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

package com.globalmentor.java;

import com.globalmentor.text.FormatUtilities;

/**Utilities for manipulating bytes.
@author Garret Wilson
*/
public class Bytes
{

	/**A shared empty array of bytes.*/
	public final static byte[] NO_BYTES=new byte[0];

	/**This class cannot be publicly instantiated.*/
	private Bytes() {}

	/**Converts an array of bytes into a hex string, with each character pair
		representing the hexadecimal value of the byte.
	@param bytes The values to convert.
	@return A lowercase string with hexadecimal digits, each pair representing a byte in the
		byte array.
	*/
	public static String toHexString(final byte[] bytes)
	{
		return FormatUtilities.formatHex(new StringBuilder(), bytes).toString();	//format the hex into a string buffer and return the string version
	}

}