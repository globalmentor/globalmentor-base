package com.garretwilson.lang;

/**Utilities for manipulating long objects.
@author Garret Wilson
*/
public class LongUtilities
{

	/**This class cannot be publicly instantiated.*/
	private LongUtilities() {}

	/**Returns a hash code for a long value.
	This implementation returns the same value used by {@link Long#hashCode()}.
	@param value The value for which a hash code should be returned.
	@return The hash code of the long value.
	*/
	public static int hashCode(final long value)
	{
		return (int)(value^(value>>>32));
	}

	/**Converts a long into a hex string with the specified number of digits.
	@param value The value to convert.
	@param length The number of digits the returned string should have.
	@return Lowercase hex version of the given value with the correct number of
		digits, using zeros to pad the left of the string to the correct length.
	*/
	public static String toHexString(final long value, final int length)
	{
		  //convert the integer to hex, then make the string the correct length by padding the beginning with zeros
		return StringUtilities.makeStringLength(Long.toHexString(value), length, '0', 0);
	}

	/**Compares two longs for order.
	@return a negative integer, zero, or a positive integer as this object is less than, equal to, or greater than the specified object.
	*/
	public static int compare(final long l1, final long l2)
	{
		return l1<l2 ? -1 : (l1>l2 ? 1 : 0);	//perform the comparison manually, because subtracting longs and returning an integer can result in overflow
	}
}