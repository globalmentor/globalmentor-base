/*
 * Copyright © 1996-2012 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import java.util.Random;

import javax.annotation.*;

import com.globalmentor.text.ASCII;
import com.globalmentor.text.TextFormatter;

/**
 * Utilities for manipulating bytes.
 * @author Garret Wilson
 */
public class Bytes {

	/** A shared empty array of bytes. */
	public static final byte[] NO_BYTES = new byte[0];

	/** This class cannot be publicly instantiated. */
	private Bytes() {
	}

	/**
	 * Converts an array of bytes into a hex string, with each character pair representing the hexadecimal value of the byte.
	 * @param bytes The values to convert.
	 * @return A lowercase string with hexadecimal digits, each pair representing a byte in the byte array.
	 */
	public static String toHexString(final byte[] bytes) { //TODO switch to Java 17 `HexFormat`
		return TextFormatter.formatHex(bytes); //format the hex into a string buffer and return the string version TODO make more efficient; see https://stackoverflow.com/a/21178195
	}

	/**
	 * Converts a sequence of hex values to bytes, without regard to case.
	 * @implNote This implementation modified from an <a href="https://stackoverflow.com/a/140861">answer on Stack Overflow</a>.
	 * @param hex The two-digit hex values to convert.
	 * @return The equivalent bytes of the hex characters.
	 * @throws IllegalArgumentException if a hex value is missing one of its pairs (i.e. the sequence length is odd) or if a hex representation contains an
	 *           invalid character.
	 */
	public static byte[] fromHexString(@Nonnull final CharSequence hex) { //TODO switch to Java 17 `HexFormat`
		final int length = hex.length();
		checkArgument((length & 1) == 0, "String must have an even number of characters. (Hex digits come in pairs.)");
		final int byteCount = length / 2;
		final byte[] bytes = new byte[byteCount];
		for(int i = 0; i < length; i += 2) {
			bytes[i / 2] = (byte)((ASCII.valueOfHexDigit(hex.charAt(i)) << 4) + ASCII.valueOfHexDigit(hex.charAt(i + 1)));
		}
		return bytes;
	}

	/**
	 * Creates an array of random bytes.
	 * @implSpec This implementation delegates to {@link #generateRandom(int, Random)} using a default {@link Random} instance.
	 * @param length The number of bytes to create.
	 * @return A new array of the given length filled with random bytes.
	 * @throws IllegalArgumentException if the given length is negative.
	 * @see Random#nextBytes(byte[])
	 */
	public static byte[] generateRandom(@Nonnegative final int length) {
		return generateRandom(length, new Random());
	}

	/**
	 * Creates an array of random bytes.
	 * @param length The number of bytes to create.
	 * @param random The random number generator to use.
	 * @return A new array of the given length filled with random bytes.
	 * @throws NullPointerException if the given random number generator is <code>null</code>.
	 * @throws IllegalArgumentException if the given length is negative.
	 * @see Random#nextBytes(byte[])
	 */
	public static byte[] generateRandom(@Nonnegative final int length, @Nonnull final Random random) {
		checkArgumentNotNegative(length);
		final byte[] bytes = new byte[length]; //create a new array of bytes
		random.nextBytes(bytes); //fill the byte array with random values
		return bytes;
	}

	/**
	 * Determines if the given byte array starts with the specified prefix. Neither byte array is modified by this method.
	 * @param bytes The bytes being examined.
	 * @param prefix The prefix to compare with the given bytes.
	 * @return Whether the given bytes start with the specified prefix.
	 */
	public static boolean startsWith(final byte[] bytes, final byte[] prefix) {
		if(bytes.length < prefix.length) { //if the array of bytes is not as long as the specified prefix
			return false; //there aren't enough bytes to compare
		}
		for(int i = prefix.length - 1; i >= 0; --i) { //look at each of the prefix bytes
			if(bytes[i] != prefix[i]) { //if these bytes don't match
				return false; //show that there is a mismatch
			}
		}
		return true; //the array of bytes passed all the tests
	}

}
