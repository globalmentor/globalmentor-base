package com.garretwilson.lang;

import com.garretwilson.text.FormatUtilities;

/**Utilities for manipulating bytes.
@author Garret Wilson
*/
public class ByteUtilities
{

	/**This class cannot be publicly instantiated.*/
	private ByteUtilities() {}

	/**Converts a string of hexadecimal characters to an array of bytes, with each
		character pair being converted into one byte.
	@param hexString The string of hexadecimal digits.
	@return A byte array with each byte representing a pair of hexadecimal digits.
	@exception InvalidParameterException Thrown if the string is not a valid
		string of hexadecimal digits with an even number of characters.
	*/
/*G***fix	if needed
	public static Byte[] toHexByteArray(final String hexString) throws InvalidParameterException
	{
		if(hexString.length()&1!=0) //if this hex string

		for(int i=0

	}
*/

/*G***del if not needed; now using com.garretwilson.util.Base64
	protected final static char[] ALPHABET=
			{
				'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',		//  0 to  7
				'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',		//  8 to 15
				'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',		// 16 to 23
				'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',		// 24 to 31
				'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',		// 32 to 39
				'o', 'p', 'q', 'r', 's', 't', 'u', 'v',		// 40 to 47
				'w', 'x', 'y', 'z', '0', '1', '2', '3',		// 48 to 55
				'4', '5', '6', '7', '8', '9', '+', '/' 		// 56 to 63
			};
*/

	/**Base64-encodes an array of bytes into a string.
	<p>Based upon Base64Encoder version 1998-06-08
		by <a href="mailto:croft@alumni.caltech.edu">David W. Croft</a>.</p>
	@param bytes The values to convert.
	@return A string of base64-encoded bytes.
	@see RFC 1421
	@see http://www.javaworld.com/javaworld/javatips/jw-javatip47.html
	*/
/*G***del if not needed; now using com.garretwilson.util.Base64
	public static String encodeBase64(final byte[] bytes)
	{
		int bits24;
		int bits6;
		char[] out= new char[((bytes.length - 1) / 3 + 1) * 4];
		int outIndex= 0;
		int i= 0;
		while ((i + 3) <= bytes.length)
		{
			// store the octets
			bits24= (bytes[i++] & 0xFF) << 16;
			bits24 |= (bytes[i++] & 0xFF) << 8;
			bits24 |= (bytes[i++] & 0xFF) << 0;
			bits6= (bits24 & 0x00FC0000) >> 18;
			out[outIndex++]= ALPHABET[bits6];
			bits6= (bits24 & 0x0003F000) >> 12;
			out[outIndex++]= ALPHABET[bits6];
			bits6= (bits24 & 0x00000FC0) >> 6;
			out[outIndex++]= ALPHABET[bits6];
			bits6= (bits24 & 0x0000003F);
			out[outIndex++]= ALPHABET[bits6];
		}
		if (bytes.length - i == 2)
		{
			// store the octets
			bits24= (bytes[i] & 0xFF) << 16;
			bits24 |= (bytes[i + 1] & 0xFF) << 8;
			bits6= (bits24 & 0x00FC0000) >> 18;
			out[outIndex++]= ALPHABET[bits6];
			bits6= (bits24 & 0x0003F000) >> 12;
			out[outIndex++]= ALPHABET[bits6];
			bits6= (bits24 & 0x00000FC0) >> 6;
			out[outIndex++]= ALPHABET[bits6];
			// padding
			out[outIndex++]= '=';
		}
		else if (bytes.length - i == 1)
		{
			// store the octets
			bits24= (bytes[i] & 0xFF) << 16;
			bits6= (bits24 & 0x00FC0000) >> 18;
			out[outIndex++]= ALPHABET[bits6];
			bits6= (bits24 & 0x0003F000) >> 12;
			out[outIndex++]= ALPHABET[bits6];
			// padding
			out[outIndex++]= '=';
			out[outIndex++]= '=';
		}
		return new String(out);
	}
*/
	
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