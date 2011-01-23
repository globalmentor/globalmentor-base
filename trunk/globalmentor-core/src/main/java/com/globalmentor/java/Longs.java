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

	/**Checks to make sure that a given index is within the given range.
	@param index The index to check.
	@param rangeMin The minimum range index, inclusive.
	@param rangeMax The maximum range index, exclusive.
	@exception IndexOutOfBoundsException if the index is less than the range minimum, or equal to or greater than the range maximum.
	@return The given index.
	*/
	public static long checkIndexBounds(final long index, final long rangeMin, final long rangeMax)
	{
		if(index<rangeMin || index>=rangeMax)	//if the index not within its bounds
		{
			throw new IndexOutOfBoundsException("Index out of bounds: "+index);
		}
		return index;	//return the index, which has been determined to be in bounds
  }

	/**Checks to make sure that a given value is within the given range.
	@param value The value to check.
	@param rangeMin The minimum range value, inclusive.
	@param rangeMax The maximum range value, inclusive.
	@exception IllegalArgumentException if the value is less than the range minimum or greater than the range maximum.
	@return The given value.
	*/
	public static long checkRange(final long value, final long rangeMin, final long rangeMax)
	{
		if(value<rangeMin || value>rangeMax)	//if the value not within the range
		{
			throw new IllegalArgumentException("Value "+value+" is not within the range "+rangeMin+" to "+rangeMax);
		}
		return value;	//return the value, which has been determined to be within the range
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