/*
 * Copyright © 1996-2013 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import static com.globalmentor.java.Conditions.*;

/**
 * Utilities for manipulating long objects.
 * @author Garret Wilson
 */
public class Longs
{

	/** The shared empty array of longs. */
	public final static long[] NO_LONGS = new long[0];

	/** This class cannot be publicly instantiated. */
	private Longs()
	{
	}

	/**
	 * Returns a hash code for a long value. This implementation returns the same value used by {@link Long#hashCode()}.
	 * @param value The value for which a hash code should be returned.
	 * @return The hash code of the long value.
	 */
	public static int hashCode(final long value)
	{
		return (int)(value ^ (value >>> 32));
	}

	/**
	 * Converts a long into a hex string with the specified number of digits.
	 * @param value The value to convert.
	 * @param length The number of digits the returned string should have.
	 * @return Lowercase hex version of the given value with the correct number of digits, using zeros to pad the left of the string to the correct length.
	 */
	public static String toHexString(final long value, final int length)
	{
		//convert the integer to hex, then make the string the correct length by padding the beginning with zeros
		return Strings.makeStringLength(Long.toHexString(value), length, '0', 0);
	}

	/**
	 * Converts a long value to an integer, checking for overflow.
	 * 
	 * @param value The value to convert.
	 * @return The long value as an integer.
	 * @throws IllegalArgumentException if the given value is smaller than {@link Integer#MIN_VALUE} or greater than {@link Integer#MAX_VALUE}.
	 */
	public static int toInt(final long value)
	{
		return (int)checkArgumentRange(value, Integer.MIN_VALUE, Integer.MAX_VALUE);
	}

	/**
	 * Compares two longs for order.
	 * @return a negative integer, zero, or a positive integer as this object is less than, equal to, or greater than the specified object.
	 */
	public static int compare(final long l1, final long l2)
	{
		return l1 < l2 ? -1 : (l1 > l2 ? 1 : 0); //perform the comparison manually, because subtracting longs and returning an integer can result in overflow
	}
}