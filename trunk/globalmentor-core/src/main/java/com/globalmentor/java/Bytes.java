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

import com.globalmentor.text.TextFormatter;

/**
 * Utilities for manipulating bytes.
 * @author Garret Wilson
 */
public class Bytes
{

	/** A shared empty array of bytes. */
	public final static byte[] NO_BYTES = new byte[0];

	/** This class cannot be publicly instantiated. */
	private Bytes()
	{
	}

	/**
	 * Converts an array of bytes into a hex string, with each character pair representing the hexadecimal value of the byte.
	 * @param bytes The values to convert.
	 * @return A lowercase string with hexadecimal digits, each pair representing a byte in the byte array.
	 */
	public static String toHexString(final byte[] bytes)
	{
		return TextFormatter.formatHex(bytes); //format the hex into a string buffer and return the string version
	}

	/**
	 * Creates an array of random bytes.
	 * @param length The number of bytes to create.
	 * @return A new array of the given length filled with random bytes.
	 * @throws IllegalArgumentException if the given length is negative.
	 */
	public static byte[] createRandom(final int length)
	{
		return createRandom(length, new Random());
	}

	/**
	 * Creates an array of random bytes.
	 * @param length The number of bytes to create.
	 * @param random The random number generator to use.
	 * @return A new array of the given length filled with random bytes.
	 * @throws NullPointerException if the given random number generator is <code>null</code>.
	 * @throws IllegalArgumentException if the given length is negative.
	 */
	public static byte[] createRandom(final int length, final Random random)
	{
		checkArgumentNotNegative(length);
		final byte[] bytes = new byte[length]; //create a new array of bytes
		random.nextBytes(bytes); //fill the byte array with random values
		return bytes;
	}

}