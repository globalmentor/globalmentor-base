/* Copyright © 2007 GlobalMentor, Inc. <http://www.globalmentor.com/>
 * All Rights Reserved.
 * 
 * Use is subject to the BSD-style license at
 * <https://svn.globalmentor.com/java/src/com/globalmentor/license.txt>.
 */

package com.globalmentor.java;

import com.garretwilson.text.FormatUtilities;

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