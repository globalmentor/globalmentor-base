/*
 * Copyright © 1996-2013 GlobalMentor, Inc. <https://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.globalmentor.java;

import static com.globalmentor.java.Conditions.*;

import javax.annotation.*;

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
	 * Converts a long to a hex string with the specified number of digits.
	 * @param value The value to convert.
	 * @param length The number of digits the returned string should have.
	 * @return Lowercase hex version of the given value with the correct number of digits, using zeros to pad the left of the string to the correct length.
	 */
	public static String toHexString(final long value, final int length) {
		//convert the integer to hex, then make the string the correct length by padding the beginning with zeros
		return Strings.forceLength(Long.toHexString(value), length, '0', 0);
	}

	/**
	 * Determines the a long value equivalent of a big-endian sequence of bytes, equivalent to that produced by {@link java.nio.ByteBuffer#putLong(long)}, stored
	 * in the given array.
	 * @apiNote This method requires exactly the number of bytes necessary. If some of the bytes should be ignored, use {@link #fromBytes(byte[], int)}.
	 * @implSpec This implementation delegates to {@link #fromBytes(byte[], int)}.
	 * @param bytes The array in which the bytes are stored.
	 * @return The long value stored as bytes.
	 * @throws IllegalArgumentException if the number of bytes given is not exactly {@value Long#BYTES}.
	 * @throws ArrayIndexOutOfBoundsException if the given array does not have sufficient bytes to store a long value.
	 */
	public static long fromBytes(@Nonnull final byte[] bytes) {
		checkArgument(bytes.length == Long.BYTES, "Exactly %d bytes are required to define a long value.", Long.BYTES);
		return fromBytes(bytes, 0);
	}

	/**
	 * Determines the a long value equivalent of a big-endian sequence of bytes, equivalent to that produced by {@link java.nio.ByteBuffer#putLong(int, long)},
	 * stored in the given array at the given index.
	 * @param bytes The array in which the bytes are stored.
	 * @param index The initial index in the array at which the bytes are stored.
	 * @return The long value stored as bytes.
	 * @throws ArrayIndexOutOfBoundsException if the given array, starting at the given index, does not have sufficient bytes to store a long value.
	 */
	public static long fromBytes(@Nonnull final byte[] bytes, final int index) {
		long value = 0;
		for(int i = index, len = index + Long.BYTES; i < len; i++) {
			value = (value << 8) | (bytes[i] & 0xFF);
		}
		return value;
	}

	/**
	 * Converts a long value to its equivalent big-endian sequence of bytes, equivalent to <code>ByteBuffer.allocate(8).putLong(value).array()</code>.
	 * @implNote This implementation delegates to {@link #toBytes(long, byte[])}.
	 * @param value The value to convert to a sequence of bytes.
	 * @return The sequence of bytes representing the long value in network byte order.
	 */
	public static byte[] toBytes(final long value) {
		return toBytes(value, new byte[Long.BYTES]);
	}

	/**
	 * Converts a long value to its equivalent big-endian sequence of bytes, equivalent to {@link java.nio.ByteBuffer#putLong(long)}, storing the bytes in the
	 * given array at the first index.
	 * @implSpec This implementation delegates to {@link #toBytes(long, byte[], int)}.
	 * @param value The value to convert to a sequence of bytes.
	 * @param bytes The array in which to store the bytes.
	 * @return The given array of bytes with the long value stored.
	 * @throws ArrayIndexOutOfBoundsException if the given array does not have sufficient bytes to store a long value.
	 */
	public static byte[] toBytes(final long value, @Nonnull final byte[] bytes) {
		return toBytes(value, bytes, 0);
	}

	/**
	 * Converts a long value to its equivalent big-endian sequence of bytes, equivalent to {@link java.nio.ByteBuffer#putLong(int, long)}, storing the bytes in
	 * the given array at the given index.
	 * @param value The value to convert to a sequence of bytes.
	 * @param bytes The array in which to store the bytes.
	 * @param index The index in the array at which to begin storing the bytes.
	 * @return The given array of bytes with the long value stored.
	 * @throws ArrayIndexOutOfBoundsException if the given array, starting at the given index, does not have sufficient bytes to store a long value.
	 */
	public static byte[] toBytes(long value, @Nonnull final byte[] bytes, final int index) {
		for(int i = index + Long.BYTES - 1; i >= index; i--) {
			bytes[i] = (byte)(value & 0xFF);
			value = value >>> Byte.SIZE;
		}
		return bytes;
	}

	/**
	 * Converts a big-endian sequence of bytes to the represented long value, equivalent to <code>ByteBuffer.wrap(bytes).getLong()</code>.
	 * @implNote Modified from code in an <a href="https://stackoverflow.com/a/27610608">answer on Stack Overflow</a>.
	 * @param bytes The bytes to convert to a long.
	 * @return The long value represented by the network byte order sequence of bytes.
	 * @deprecated to be removed in favor of {@link #fromBytes(byte[])}.
	 */
	@Deprecated(forRemoval = true)
	public static long toLong(final byte[] bytes) {
		return fromBytes(bytes);
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
