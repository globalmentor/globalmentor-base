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
public class Longs {

	/** The shared empty array of longs. */
	public static final long[] NO_LONGS = new long[0];

	/** This class cannot be publicly instantiated. */
	private Longs() {
	}

	/**
	 * Returns a hash code for a long value. This implementation returns the same value used by {@link Long#hashCode()}.
	 * @param value The value for which a hash code should be returned.
	 * @return The hash code of the long value.
	 */
	public static int hashCode(final long value) {
		return (int)(value ^ (value >>> 32));
	}

	/**
	 * Converts a long into a hex string with the specified number of digits.
	 * @param value The value to convert.
	 * @param length The number of digits the returned string should have.
	 * @return Lowercase hex version of the given value with the correct number of digits, using zeros to pad the left of the string to the correct length.
	 */
	public static String toHexString(final long value, final int length) {
		//convert the integer to hex, then make the string the correct length by padding the beginning with zeros
		return Strings.forceLength(Long.toHexString(value), length, '0', 0);
	}

	/**
	 * Converts a long value into its equivalent big-endian sequence of bytes, equivalent to <code>ByteBuffer.allocate(8).putLong(value).array()</code>.
	 * @implNote Modified from code in an <a href="https://stackoverflow.com/a/27610608">answer on Stack Overflow</a>.
	 * @param value The value to convert to a sequence of bytes.
	 * @return The sequence of bytes representing the long value in network byte order.
	 */
	public static byte[] toBytes(long value) {
		return new byte[] {(byte)(value >> 8 * 7), (byte)(value >> 8 * 6), (byte)(value >> 8 * 5), (byte)(value >> 8 * 4), (byte)(value >> 8 * 3),
				(byte)(value >> 8 * 2), (byte)(value >> 8 * 1), (byte)value};
	}

	/**
	 * Converts a big-endian sequence of bytes into the represented long value, equivalent to <code>ByteBuffer.wrap(bytes).getLong()</code>.
	 * @implNote Modified from code in an <a href="https://stackoverflow.com/a/27610608">answer on Stack Overflow</a>.
	 * @param bytes The bytes to convert to a long.
	 * @return The long value represented by the network byte order sequence of bytes.
	 */
	public static long toLong(final byte[] bytes) {
		return (bytes[0] & 0xFFL) << 8 * 7 | (bytes[1] & 0xFFL) << 8 * 6 | (bytes[2] & 0xFFL) << 8 * 5 | (bytes[3] & 0xFFL) << 8 * 4 | (bytes[4] & 0xFFL) << 8 * 3
				| (bytes[5] & 0xFFL) << 8 * 2 | (bytes[6] & 0xFFL) << 8 * 1 | (bytes[7] & 0xFFL);
	}

	/**
	 * Converts a long value to an integer, checking for overflow.
	 * 
	 * @param value The value to convert.
	 * @return The long value as an integer.
	 * @throws IllegalArgumentException if the given value is smaller than {@link Integer#MIN_VALUE} or greater than {@link Integer#MAX_VALUE}.
	 */
	public static int toInt(final long value) {
		return (int)checkArgumentRange(value, Integer.MIN_VALUE, Integer.MAX_VALUE);
	}

	/**
	 * Compares two longs for order.
	 * @param l1 The first long to compare.
	 * @param l2 The second long to compare.
	 * @return a negative integer, zero, or a positive integer as this object is less than, equal to, or greater than the specified object.
	 */
	public static int compare(final long l1, final long l2) {
		return l1 < l2 ? -1 : (l1 > l2 ? 1 : 0); //perform the comparison manually, because subtracting longs and returning an integer can result in overflow
	}
}
