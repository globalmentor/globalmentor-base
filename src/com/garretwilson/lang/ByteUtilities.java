package com.garretwilson.lang;

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

	/**Converts an array of bytes into a hex string, with each character pair
		representing the hexadecimal value of the byte.
	@param byteArray The value to convert.
	@return A string with hexadecimal digits each pair representing a byte in the
		byte array.
	*/
/*G***fix if needed
	public static String toHexString(final Byte[] byteArray)
	{
		final StringBuffer stringBuffer=new StringBuffer(byteArray.length*2); //create a string buffer large enough to hold the hex digits
		for(int i=0; i<byteArray.length; ++i)  //look at each of the bytes
		{
		  stringBuffer.append(Integer.toHexString(byteArray[i]));  //convert the byte to a hex string and add it to our string buffer
		}
		return stringBuffer.toString(); //return the string we constructed
	}
*/

}