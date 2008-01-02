/* Copyright © 2007 GlobalMentor, Inc. <http://www.globalmentor.com/>
 * All Rights Reserved.
 * 
 * Use is subject to the BSD-style license at
 * <https://svn.globalmentor.com/java/src/com/globalmentor/license.txt>.
 */

package com.globalmentor.java;

/**Utilities for manipulating long objects.
@author Garret Wilson
*/
public class Longs
{

	/**This class cannot be publicly instantiated.*/
	private Longs() {}

	/**Returns a hash code for a long value.
	This implementation returns the same value used by {@link Long#hashCode()}.
	@param value The value for which a hash code should be returned.
	@return The hash code of the long value.
	*/
	public static int hashCode(final long value)
	{
		return (int)(value^(value>>>32));
	}

	/**Checks to make sure that a given value is not smaller than the given minimum.
	@param value The value to check.
	@param rangeMin The minimum range value, inclusive.
	@exception IllegalArgumentException if the value is less than the range minimum.
	@return The given value.
	*/
	public static long checkMinimum(final long value, final long rangeMin)
	{
		if(value<rangeMin)	//if the value not within the range
		{
			throw new IllegalArgumentException("Value "+value+" cannot be less than "+rangeMin);
		}
		return value;	//return the value, which has been determined to be within the range
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
		return Strings.makeStringLength(Long.toHexString(value), length, '0', 0);
	}

	/**Compares two longs for order.
	@return a negative integer, zero, or a positive integer as this object is less than, equal to, or greater than the specified object.
	*/
	public static int compare(final long l1, final long l2)
	{
		return l1<l2 ? -1 : (l1>l2 ? 1 : 0);	//perform the comparison manually, because subtracting longs and returning an integer can result in overflow
	}
}